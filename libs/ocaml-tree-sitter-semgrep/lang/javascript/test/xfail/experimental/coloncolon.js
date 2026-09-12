/*
  This is the example from
    https://github.com/tc39/proposal-bind-operator
  which is a 2018 proposal for a bind operator '::'.

  Not sure about its history or why it's found in some javascript out there.
*/
import { map, takeWhile, forEach } from "iterlib";

getPlayers()
::map(x => x.character())
::takeWhile(x => x.strength > 100)
::forEach(x => console.log(x));
