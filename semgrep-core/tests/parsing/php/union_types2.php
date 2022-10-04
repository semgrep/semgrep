<?php
//https://github.com/returntocorp/semgrep/issues/3588
class Autoconfigure
{
    public function __construct(
        public ?array $tags = null,
        public ?array $calls = null,
        public ?array $bind = null,
        public bool|string|null $lazy = null,
        public ?bool $public = null,
        public ?bool $shared = null,
        public ?bool $autowire = null,
        public ?array $properties = null,
        public array|string|null $configurator = null,
    ) {
    }
}
