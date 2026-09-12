/*
  semgrep-dockerfile

  Extends the standard dockerfile grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-dockerfile/grammar');

module.exports = grammar(base_grammar, {
  name: 'dockerfile',

  conflicts: ($, previous) => previous.concat([
  ]),

  rules: {
    _instruction: ($, previous) => choice(
      $.semgrep_ellipsis,
      $.semgrep_metavariable,
      previous
    ),

    // overrides the original definition
    json_string_array: ($) =>
      seq(
        "[",
        optional(
          seq($._array_element, repeat(seq(",", $._array_element)))
        ),
        "]"
      ),

    _array_element: ($) => choice(
      $.json_string,
      $.semgrep_ellipsis,
      $.semgrep_metavariable
    ),

    // redefinition
    arg_instruction: ($, previous) => seq(
      alias(/[aA][rR][gG]/, "ARG"),
      field(
        "name",
        alias(
          choice(
            $.semgrep_metavariable,
            /[a-zA-Z0-9_]+/
          ),
          $.unquoted_string
        )
      ),
      optional(
        seq(
          token.immediate("="),
          field(
            "default",
            choice(
              $.double_quoted_string,
              $.single_quoted_string,
              $.unquoted_string
            )
          )
        )
      )
    ),

    // override
    label_pair: ($, previous) => choice(
      $.semgrep_ellipsis,
      seq(
        field(
          "key",
          choice(
            $.semgrep_metavariable,
            alias(/[-a-zA-Z0-9\._]+/, $.unquoted_string),
            $.double_quoted_string,
            $.single_quoted_string
          )
        ),
        token.immediate("="),
        field(
          "value",
          choice(
            $.double_quoted_string,
            $.single_quoted_string,
            $.unquoted_string
          ))
      )
    ),

    // TODO: find a way to support metavariables as env keys.
    env_pair: ($, previous) => choice(
      $.semgrep_ellipsis,
      previous
    ),

    expose_port: ($, previous) => choice(
      $.semgrep_ellipsis,
      previous
    ),

    // override
    healthcheck_instruction: ($) =>
      seq(
        alias(/[hH][eE][aA][lL][tT][hH][cC][hH][eE][cC][kK]/, "HEALTHCHECK"),
        choice(
          $.semgrep_metavariable,
          // To support `HEALTHCHECK ...`
          $.semgrep_ellipsis,
          "NONE",
          // semgrep ellipsis here to support `HEALTHCHECK ... CMD echo bar`
          seq(repeat(choice($.semgrep_ellipsis, $.param)), $.cmd_instruction)
        )
      ),

    shell_command: ($, previous) => choice(
      $.semgrep_ellipsis,
      previous
    ),

    /*
      Metavariable syntax vs. ARG expansions:

      - in a semgrep pattern, $FOO is always a metavariable.
      - in a semgrep pattern, ${FOO} is always an arg expansion.
      - in a semgrep pattern, $FOO in a position where a metavariable
        is not allowed results in an error.

     TODO: support metavariables and ellipses in a bunch of places
    */
    semgrep_metavariable: $ => /\$[A-Z_][A-Z_0-9]*/,
    semgrep_ellipsis: $ => '...',

    /*
      TODO: more syntax ellipsis (e) or metavariables (mv):

      mv: done
      FROM extras:$CODE_VERSION       # metavariable (pattern only)
      FROM extras:${CODE_VERSION}     # parameter from ARG

      mv: done
      LABEL $NAME=$VAL

      e: done
      LABEL ... multi.label1="value1" ...

      mv: done
      MAINTAINER bob

      mv: done
      EXPOSE 80/tcp 80/udp
      EXPOSE $PORT_PROTO

      e: done
      EXPOSE ...

      mv: (tricky)
      ENV $MY_NAME=$VAL
      ENV $MY_NAME $VAL

      mv: done
      ENV MY_NAME=$VAL

      e: done
      ENV ... MY_NAME="John Doe" ...

      e, mv: done
      ADD a b /somedir/
      COPY a b /somedir/

      e, mv: done
      ADD --chown=55:mygroup a b /somedir/
      COPY --chown=55:mygroup a b /somedir/

      e, mv: done
      VOLUME /var/log /var/db

      mv: done
      USER patrick
      USER patrick:star

      mv: done
      WORKDIR /a

      mv: done
      ARG pat
      ARG buildno=42

      mv: done
      STOPSIGNAL SIGUSR1
      STOPSIGNAL $SIGNAL

      mv: done
      HEALTHCHECK --interval=5m --timeout=3s CMD echo
      HEALTHCHECK NONE
      HEALTHCHECK $X
    */
  }
});
