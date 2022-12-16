% -*- prolog -*-

%---------------------------------------------------------------------------
% Prelude
%---------------------------------------------------------------------------

% This file is the basis of an interactive tool a la SQL to query
% information about the structure of a codebase (the inheritance tree,
% the call graph, the data graph), for instance "What are all the
% children of class Foo?". The data is the code. The query language is
% Prolog (http://en.wikipedia.org/wiki/Prolog), a logic-based
% programming language used mainly in AI but also popular in database
% (http://en.wikipedia.org/wiki/Datalog). The particular Prolog
% implementation we use for now is SWI-prolog
% (http://www.swi-prolog.org/pldoc/refman/). We've chosen Prolog over
% SQL because it's really easy to define recursive predicates like
% children/2 (see below) in Prolog, and such predicates are necessary
% when dealing with object-oriented codebase.
%
% This tool is inspired by a similar tool for Java called JQuery
% (http://jquery.cs.ubc.ca/, nothing to do with the JS library), itself
% inspired by CIA (C Information Abstractor). The code below is mostly
% generic (programming language agnostic) but it was tested mainly
% on PHP, Java (and its bytecode), and OCaml code (see
% lang_php/analyze/foundation/unit_prolog_php.ml,
% lang_bytecode/analyze/unit_analyze_bytecode.ml, and
% lang_ml/analyze/unit_analyze_ml.ml).
%
% This file assumes the presence of another file, facts.pl, containing
% the actual "database" of facts about a codebase. There is potentially
% an infinite numbers of predicates we could define. For instance
% does the method contains a for loop, does it call '+', etc. But for now
% we focus on predicates related to entities, to names, e.g. defs and uses
% of functions/classes/etc.
% Here are the predicates that should be defined in facts.pl:
%
%  - entities: kind/2 with the
%    function/method, constant, class/interface/trait, field
%    atoms.
%      ex: kind('array_map', function).
%      ex: kind('Preparable', class).
%      ex: kind(('Preparable', 'gen'), method).
%      ex: kind((Preparable', '__count'), field).
%      ex: kind((Preparable', 'OK'), constant).
%    The identifier for a function is its name in a string and for
%    class members a pair with the name of the class and then the member name,
%    both in a string. We don't differentiate methods from static methods;
%    the static/1 predicate below can be used for that (same for fields).
%    Note that for fields the name of the field does not contain the $ because
%    when used, as in '$this->field, there is no $.
%
%  - callgraph: docall/3, special/1 with the function/method/class atoms
%    to differentiate regular function calls, method calls, and class
%    instantiations via new (see also the calls/2 infix operator).
%      ex: docall('foo', 'bar', function).
%      ex: docall(('A', 'foo'), 'toInt', method).
%      ex: docall('foo', ':x:frag', class).
%    Note that for method calls we actually don't resolve to which class
%    the method belongs to (that would require to leverage results from
%    an interprocedural static analysis) unless it's a static method call.
%    Note that we use 'docall' and not 'call' because call is a
%    reserved predicate in Prolog.
%    A new atom 'special' can be used to indicate calls to special functions
%    taking entities as parameters. For instance a wrapper to 'new' in
%    a dynamic language like PHP.
%      ex: docall('foo', ('new_wrapper','A'), special).
%    and a special/1 predicate is used to remember all those special functions.
%      ex: special('new_wrapper').
%
%
%  - exception graph: throw/2, catch/2.
%     ex: throw('foo', 'ViolationException').
%     ex: catch('bar', 'Exception').
%
%  - datagraph: use/4 with the field/array atoms to differentiate access
%    to object members, and access to fields of an array (often because people
%    abuse arrays to represent records), and the read/write atoms to
%    indicate in which position the field is used.
%     ex: use('foo', 'count', field, read).
%     ex: use(('A','foo'), 'name', array, write).
%
%  - types: type/2, parameter/4, return/2, arity/2
%     ex: type('foobar', 'int').
%     ex: parameter('foo', 0, '$first_param_name', 'int')
%     ex: return('foo', 'int')
%     ex: arity('foobar', 3).
%     ex: arity(('Preparable', 'gen'), 0).
%
%  - properties: static/1, abstract/1, final/1, is_public/1, is_private/1,
%     is_protected/1,  async/1
%      ex: static(('Filesystem', 'readFile')).
%      ex: abstract('AbstractTestCase').
%      ex: is_public(('Preparable', 'gen')).
%    We use 'is_public' and not 'public' because public is a reserved keyword
%    in Prolog.
%
%  - inheritance: extends/2, implements/2, mixins/2
%      ex: extends('EntPhoto', 'Ent').
%      ex: implements('MyTest', 'NeedSqlShim').
%      ex: mixins('MyTest', 'TraitHaveFeedback').
%    See also the children/2, parent/2, related/2, isa/2, inherits/2,
%    reuses/2, predicates defined below, where isa and inherits are
%    infix operators.
%
%  - include/require: include/2, require_module/2
%      ex: include('wap/index.php', 'flib/core/__init__.php').
%      ex: require_module('flib/core/__init__.php', 'core/db').
%    We don't differentiate 'include' from 'require'. Note that include works
%    on desugared flib code so the require_module() are translated in
%    their equivalent includes. Finally path are resolved statically
%    when we can, so for instance include $THRIFT_ROOT . '...' is resolved
%    in its final path form 'lib/thrift/...'.
%
%  - yield/1.
%
%  - position: at/3
%      ex: at(('Preparable', 'gen'), 'flib/core/preparable.php', 10).
%
%  - file information: file/2, hh/2
%       ex: file('wap/index.php', ['wap','index.php']).
%       ex: hh('flib/x/foo.php', strict).
%    By having a list one then use member/3 to select subparts of the codebase
%    easily (or use explode_file/2).
%
% related work:
%  - jquery, tyruba
%  - CIA
%  - ODASA, codequest
%  - LFS/PofFS
%
% limitations:
%  - in the case of PHP, the language is case insensitive but we actually
%    generate facts where the case matters. You can use downcase_atom/2
%    to try to do case insensitive search, e.g.
%      ? kind(X, class), downcase_atom(X, Y), Y = 'exception', <query with X>
%    but this will work only for the first level. If in the code
%    some extends or implements are using the wrong case, you're lost.

%---------------------------------------------------------------------------
% How to run/compile
%---------------------------------------------------------------------------

% Generates a /tmp/facts.pl for your codebase for your programming language
% (e.g. with pfff_db_heavy -gen_prolog_db /tmp/pfff_db /tmp/facts.pl)
% and then:
%
%   $ swipl -s /tmp/facts.pl -f prolog_code.pl
%
% If you want to test a new predicate you can do for instance:
%
%  $ swipl -s /tmp/facts.pl -f prolog_code.pl -t halt --quiet -g "children(X,'Foo'), writeln(X), fail"
%
% If you want to compile a database do:
%
%   $ swipl -c /tmp/facts.pl prolog_code.pl #this will generate a 'a.out'
%
% Finally you can also use a precompiled database with:
%
%   $ cmf --prolog or /home/engshare/pfff/prolog_www
%

%---------------------------------------------------------------------------
% Inheritance
%---------------------------------------------------------------------------

extends_or_implements(Child, Parent) :-
        extends(Child, Parent).
extends_or_implements(Child, Parent) :-
        implements(Child, Parent).

extends_or_mixins(Child, Parent) :-
        extends(Child, Parent).
extends_or_mixins(Class, Trait) :-
        mixins(Class, Trait).


public_or_protected(X) :-
        is_public(X).
public_or_protected(X) :-
        is_protected(X).

method_or_field(method).
method_or_field(field).


children(Child, Parent) :-
        extends_or_implements(Child, Parent).
children(GrandChild, Parent) :-
        extends_or_implements(GrandChild, Child),
        children(Child, Parent).
children(GrandChild, Parent) :-
        mixins(GrandChild, Trait),
        children(Trait, Parent).

%aran: only extends
inherits(Child, Parent) :-
        extends(Child, Parent).
inherits(GrandChild, Parent) :-
        extends(GrandChild, Child),
        inherits(Child, Parent).

%only for traits
reuses(Child, Trait) :-
        mixins(Child, Trait).
reuses(GrandChild, Trait) :-
        extends(GrandChild, Child),
        reuses(Child, Trait).

parent(X, Y) :-
        children(Y, X).

% bidirectional
related(X, Y) :-
        children(X, Y).
related(X, Y) :-
        children(Y, X).

%---------------------------------------------------------------------------
% Class information
%---------------------------------------------------------------------------

% one can use the same predicate in many ways in Prolog :)
method_in_class(X, Method) :-
        kind((X, Method), method).
class_defining_method(Method, X) :-
        kind((X, Method), method).

% get all methods/fields accessible from a class
% todo: for mixins it does not handle yet insteadof and as, but we should
% not use those features anyway.
method(Class, (Class, Method)) :-
        kind((Class, Method), method).
method(Class, (Class2, Method)) :-
        extends_or_mixins(Class, Parent),
        method(Parent, (Class2, Method)),
        % ensure we don't count parent implementations of overridden functions
        \+ kind((Class, Method), method),
        public_or_protected((Class2, Method)).

field(Class, (Class, Field)) :-
        kind((Class, Field), field).
field(Class, (Class2, Field)) :-
        extends_or_mixins(Class, Parent),
        field(Parent, (Class2, Field)),
        public_or_protected((Class2, Field)).

all_methods(Class) :- findall(X, method(Class, X), XS), writeln(XS).
all_fields(Class) :- findall(X, field(Class, X), XS), writeln(XS).

% for aran
at_method((Class, Method), File, Line) :-
        method(Class, (Class2, Method)),
        at((Class2, Method), File, Line).

% aran's override (shadowed methods) bad smell detector. People should use
% @override to be more explicit.
% todo: need then to extract annotations from php code and generate facts.
overrides(ChildClass, Class, Method) :-
        kind((ChildClass, Method), method),
        (inherits(ChildClass, Class) ; reuses(ChildClass, Class)),
        kind((Class, Method), method).
overrides(ChildClass, Method) :-
        overrides(ChildClass, _Class, Method).

% trait specific overriding
overrides_trait(ChildClass, Method) :-
        overrides(ChildClass, Class, Method),
        kind(Class, trait).

%---------------------------------------------------------------------------
% Callgraph
%---------------------------------------------------------------------------

%---------------------------------------------------------------------------
% Exception
%---------------------------------------------------------------------------

% todo: could try to find uncaught exception by using docall, throw, and
% catch predicates? would require a precise callgraph though.

%---------------------------------------------------------------------------
% Files
%---------------------------------------------------------------------------

explode_file(F, XS) :-
        atomic_list_concat(XS, '/', F).

%---------------------------------------------------------------------------
% Operators for erling
%---------------------------------------------------------------------------
calls(A,B) :- docall(A, B, _).
:- op(42, xfx, calls).

:- op(42, xfx, inherits).

isa(A,B) :- children(A,B).
:- op(42, xfx, isa).

%---------------------------------------------------------------------------
% Statistics
%---------------------------------------------------------------------------

% does not work very well with big data :(
%:- use_module(library('R')).
%load_r :- r_open([with(non_interactive)]).

%---------------------------------------------------------------------------
% Reporting
%---------------------------------------------------------------------------

%---------------------------------------------------------------------------
% Clown code
%---------------------------------------------------------------------------

%todo: histogram for kent of function arities :)
too_many_params(X) :-
        arity(X, N), N > 20.

include_not_www_code(X, Y) :-
        include(X, Y),
        \+ file(Y, _).

% this is what makes the callgraph for methods more complicated
same_method_in_unrelated_classes(Method, Class1, Class2) :-
        kind((Class1, Method), method),
        kind((Class2, Method), method),
        Method \= '__construct',
        Class1 \= Class2,
        \+ related(Class1, Class2).

%classes with more than 10 public methods: http://en.wikipedia.org/wiki/.QL
too_many_public_methods(X) :-
        kind(X, class),
        findall(M, (kind((X, M), method), public((X,M))), Res),
        length(Res, N),
        N > 10.

%---------------------------------------------------------------------------
% Security
%---------------------------------------------------------------------------

scary('XSS').
scary('POTENTIAL_XSS_HOLE').
scary('ToXHP_UNSAFE').

%---------------------------------------------------------------------------
% Refactoring opportunities
%---------------------------------------------------------------------------

% aran's code
could_be_final(Class) :-
  kind(Class, class),
  not(final(Class)),
  not(extends(_Child, Class)).

could_be_final(Class, Method) :-
  kind(Class, class),
  kind((Class, Method), method),
  not(final((Class, Method))),
  not(overrides(_ChildClass, Class, Method)).

% for paul
could_remove_delegate_method(Class, Method) :-
        docall((Class, Method), 'delegateToYield', method),
        not((children(Class, Parent), kind((Parent, Method), _Kind))).

%---------------------------------------------------------------------------
% checks
%---------------------------------------------------------------------------

check_exception_inheritance(X) :-
        throw(_, X),
        not(children(X, 'Exception')),
        X \= 'Exception',
        % make sure it's defined
        kind(X, class).

check_duplicated_entity(X, File1, File2, Kind) :-
        kind(X, Kind),
        at(X, File1, _),
        at(X, File2, _),
        File1 \= File2.

check_duplicated_field(Class, Class2, Var) :-
        kind((Class,Var), field),
        public_or_protected((Class, Var)),
        Class \= 'Exception',
        children(Class2, Class),
        kind((Class2, Var), field).

check_call_unexisting_method_anywhere(Caller, Method) :-
        docall(Caller, Method, method),
        not(kind((_X, Method), method)).

% for paul
wrong_public_genRender(X) :-
        kind((X, 'genRender'), _),
        children(X, 'GenXHP'),
        is_public((X, 'genRender')).

%todo:
% check for inconsistent case, e.g. Exception vs exception.
% just check if 2 classes are different but downcase to the same name


:- discontiguous mixins/2.
:- discontiguous implements/2.
