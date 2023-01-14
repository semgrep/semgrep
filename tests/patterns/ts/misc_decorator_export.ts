import { Module } from '@nestjs/common';
import { CatsController } from './cats.controller';
import { CatsService } from './cats.service';

//ERROR: match
@Module({
  controllers: [CatsController],
  providers: [CatsService],
})
export class CatsModule {}
