<?php

// http://docs.hhvm.com/manual/en/hack.typealiasing.opaquewithconstraints.php


newtype TransactionActive<T as Transactable> as T = T;

