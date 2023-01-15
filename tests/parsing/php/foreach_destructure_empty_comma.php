<?php
//https://github.com/returntocorp/semgrep/issues/2648
/**
 * All data has been read, time to process the data and build
 * meaningful return values.
 */
foreach ( $results as [ $rkey, $flags, /* length */, $casToken, $data ] ) {
	if ( $data === false || substr( $data, -2 ) !== "\r\n" ) {
		$this->_handle_error( $sock,
			'line ending missing from data block from $1' );
		return false;
	}
	$data = substr( $data, 0, -2 );
	$ret[$rkey] = $data;
                                                                            
	if ( $this->_have_zlib && $flags & self::COMPRESSED ) {
		$ret[$rkey] = gzuncompress( $ret[$rkey] );
	}
                                                                            
	/*
	 * This unserialize is the exact reason that we only want to
	 * process data after having read until "END" (instead of doing
	 * this right away): "unserialize" can trigger outside code:
	 * in the event that $ret[$rkey] is a serialized object,
	 * unserializing it will trigger __wakeup() if present. If that
	 * function attempted to read from memcached (while we did not
	 * yet read "END"), these 2 calls would collide.
	 */
	if ( $flags & self::SERIALIZED ) {
		$ret[$rkey] = $this->unserialize( $ret[$rkey] );
	} elseif ( $flags & self::INTVAL ) {
		$ret[$rkey] = intval( $ret[$rkey] );
	}
}
