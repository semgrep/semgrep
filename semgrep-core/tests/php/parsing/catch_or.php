<?php
//https://github.com/returntocorp/semgrep/issues/2650
try {
    $data = $response->toArray();
} catch (TransportExceptionInterface $e) {
    throw new TransportException('Could not reach the remote SpotHit server.', $response, 0, $e);
} catch (HttpExceptionInterface | DecodingExceptionInterface $e) {
    throw new TransportException('Unexpected reply from the remote SpotHit server.', $response, 0, $e);
}
