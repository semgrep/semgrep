local y = import '../libs/local.libsonnet';

{
  bar: y.foo,
  //bar_arr: y.foo_arr,
  //bar_comp: [v for v in x.foo_comp],
}
