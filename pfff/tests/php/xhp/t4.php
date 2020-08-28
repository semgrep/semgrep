<?php

final class :xui:button extends :ui:base {

  use XHPWithAttributeClasses;

  attribute
    :abstract:button,
    enum {'small', 'medium', 'large', 'xlarge', 'xxlarge'} size = 'small',
    enum {'special', 'confirm', 'default'} use = 'default';

  children ((%image, (pcdata | %phrase)?) | ((pcdata | %phrase), %image?));

  category %button;

}
