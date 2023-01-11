<?php

try {
    $text = $this->getBlobStore()->getBlob( $blobAddress );
    $textLen = strlen( $text );
} catch ( BlobAccessException | InvalidArgumentException $ex ) {
    // XXX: log $ex to stderr?
    $textLen = '-1';
    $text = '';
}
