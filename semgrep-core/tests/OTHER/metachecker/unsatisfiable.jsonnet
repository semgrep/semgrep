{
  rules: [
    {
      id: "eval-and-not-eval-is-bad",
      match: {
        and: [
          "eval(...)",
          { not: "eval(...)" }
        ]
      },
      message: "Bad eval and missing eval",
      languages: ["javascript"],
      severity: "ERROR"
    }
  ]
}
