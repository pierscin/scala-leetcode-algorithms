package pierscin.leetcode.utils

object TableGeneration extends App {
  ProblemsParser
    .fromPath(
      "algs/src/main/scala/pierscin/leetcode/Solution.scala"
    )
    .get
    .foreach(ps => {
      println(s"|${ps.difficulty(0)}|[${ps.title}](${ps.link})|")
    })
}
