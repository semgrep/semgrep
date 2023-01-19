/*
 * ============================================
 * Async function
 * ============================================
 */

async fn abc() {}

async fn main() {
    let x = futures.await?;
}

/*
 * ============================================
 * Await expression
 * ============================================
 */

async fn main() {
  futures.await;
  futures.await?;
  futures.await?.await?;
  futures.await?.function().await?;
}

/*
 * ============================================
 * Async Block
 * ============================================
 */

async fn main() {
  async {}
  async { let x = 10; }
  async move {}
}
