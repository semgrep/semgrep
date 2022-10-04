import utils = require('../lib/utils')
import { Request, Response } from 'express'

const db = require('../data/mongodb')

module.exports = function trackOrder () {
    return (req: Request, res: Response) => {
    const id = utils.disableOnContainerEnv() ? String(req.params.id).replace(/[^\w-]+/g, '') : req.params.id

    // ruleid: mongo-query-inject
    db.orders.find({ $where: `this.orderId === '${id}'` }).then(order => {
      const result = utils.queryResultToJson(order)
      if (result.data[0] === undefined) {
        result.data[0] = { orderId: id }
      }
      res.json(result)
    }, () => {
      res.status(400).json({ error: 'Wrong Param' })
    })
  }
}
