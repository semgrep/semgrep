// MATCH:
let recentEmails = unreadEmails.filter(\.isRecent)

// MATCH:
let recentEmails = unreadEmails.filter(\Email.isRecent)
