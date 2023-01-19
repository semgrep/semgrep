// https://github.com/returntocorp/semgrep/issues/3155
@Slow
@LogLevel("org.apache.solr.handler.ReplicationHandler=DEBUG;org.apache.solr.handler.IndexFetcher=DEBUG")
public class TestPullReplica extends SolrCloudTestCase {
  private List<Replica> restartPullReplica(DocCollection docCollection, int numPullReplicas) throws Exception {
    Slice s = docCollection.getSlices().iterator().next();
    List<Replica> pullReplicas = s.getReplicas(EnumSet.of(Replica.Type.PULL));
    // make sure a PULL replica recovers this first doc after a restart
    JettySolrRunner leaderJetty = cluster.getReplicaJetty(s.getLeader());

    // find a node with a PULL replica that's not hosting the leader
    JettySolrRunner replicaJetty = null;
    for (Replica r : pullReplicas) {
      JettySolrRunner jetty = cluster.getReplicaJetty(r);
      if (!jetty.getNodeName().equals(leaderJetty.getNodeName())) {
        replicaJetty = jetty;
        break;
      }
    }

    // stop / start the node with a pull replica
    //OK:
    if (replicaJetty != null) {
      replicaJetty.stop();
    } 

    return pullReplicas;
  }
}
