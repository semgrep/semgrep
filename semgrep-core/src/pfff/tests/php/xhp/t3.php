<?php

 $content[] .=
          <input
            type="hidden"
            name="post_form_id"
            value={get_encrypted_value_check_hash(
              SI_CSRF::getInstance()->getUser())}
          />;
