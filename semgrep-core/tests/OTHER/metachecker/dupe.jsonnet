{
  rules: [
    {
      id: "eval-is-bad",
      match: {
        and: [
          "eval(...)",
          "eval(...)"
        ]
      },
      message: "Bad eval",
      languages: ["javascript"],
      severity: "ERROR"
    }
  ]
}
