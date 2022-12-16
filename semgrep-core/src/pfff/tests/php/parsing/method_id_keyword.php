<?php

// from mediawiki/includes/libs/objectcache/serialized/SerializedValueContainer.php

class SerializedValueContainer {
	/**
	 * @param mixed $value
	 * @return bool
	 */
	public static function isSegmented( $value ) {
		return self::instanceOf( $value, self::SCHEMA_SEGMENTED );
	}

	/**
	 * @param mixed $value
	 * @param string $schema SCHEMA_* class constant
	 * @return bool
	 */
	private static function instanceOf( $value, $schema ) {
		return (
			$value instanceof stdClass &&
			property_exists( $value, self::SCHEMA ) &&
			$value->{self::SCHEMA} === $schema
		);
	}
}
