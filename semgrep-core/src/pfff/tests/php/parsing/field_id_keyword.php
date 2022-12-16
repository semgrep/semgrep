<?php
class Core extends Base\Core
{
    public function createFeeRecoveryPayout(array $input): Payout\Entity
    {


        $startTimeStamp = $input[Entity::FROM];

        $endTimeStamp = $input[Entity::TO];


    }
}
