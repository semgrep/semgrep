// Test: underscore lambda in colon-block argument
class JsBotApi:

   playable:

     val all = cacheApi.unit[List[BotJson]]:
      _.refreshAfterWrite().buildAsyncTimeout(): _ =>
        repo.getLatestBots()

// Test: context function arrow (?=>) in colon-block
class ContextFnExample:
  val handler = endpoint.serve: ?=> =>
    response.ok
