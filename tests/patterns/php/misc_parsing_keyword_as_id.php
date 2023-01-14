<?php

class SyncOrderPgRouter extends Job
{
    //parsing: array keyword used as function name
    public function array(): array
    {
        return $this->data;
    }
   
    public function handle()
    {
        //ERROR: match
        $this->trace->info(TraceCode::ORDER_DATA_SYNC_TO_PG_ROUTER,
            [
                'data' => $this->data,
                'data' => $traceRequest
            ]
        );

    


        //parsing: 'for', 'global', 'as' keywords used as constants
        $this->settingsAccessor = A::for($this->batch, Settings\Module::BATCH);
        list($this->settings[K::GLOBAL], $this->settings[K::ID_LEVEL]) = $settings;

        return json_encode([
            Constants::ASYNC_TRANSFER_RESPONSE_IDENTIFIER => [
                Constants::VERSION              => self::VERSION,
                Constants::REQUEST_REFERENCE_NO => $this->entity->getId(),
                Constants::UNIQUE_RESPONSE_NO   => PublicEntity::generateUniqueId(),
                Constants::REQ_TRANSFER_TYPE    => $this->transferType,
                Constants::STATUS_CODE          => Status::AS,
            ],
        ]);
    }
}
