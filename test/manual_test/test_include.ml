let p =  <"succ"> in p
// let p =  <"test/parsePlugin"> in p // no permissions!
// let p =  <"plugin"> in p // not such file
// let p =  <"plugin/w"> in p // no such file
// let p =  <"plugin/../test/parsePlugin"> in p // nope!
// let p =  <"plugin/ps/p"> in p //nope!
// let p =  <"plugin/succ"> in p
// let p =  <"trust"> in p // works only with plugins!