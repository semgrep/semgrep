
fun main() {
    val user = User("Alex", 1)
    println(user)                                          // 2

    val secondUser = User("Alex", 1)
    val thirdUser = User("Max", 2)

    println("user == secondUser: ${user == secondUser}")   // 3
    println("user == thirdUser: ${user == thirdUser}")

    println(user.hashCode())                               // 4
    println(thirdUser.hashCode())

    // copy() function
    println(user.copy())                                   // 5
    println(user.copy("Max"))                              // 6
    println(user.copy(id = 2))                             // 7

    println("name = ${user.component1()}")                 // 8
    println("id = ${user.component2()}")
}
