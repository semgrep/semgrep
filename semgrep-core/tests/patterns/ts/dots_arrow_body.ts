import {
  Controller,
  Get,
  Put,
  Param,
  NotFoundException,
  InternalServerErrorException,
} from "@nestjs/common";
import { getConnection, Entity } from "typeorm";

@Controller("accounts")
export class AppController3 {
  @Put("user/:id/:years")
  async celebrateUserBirthday(
    @Param("id") id: string,
    @Param("years") numberOfYearsToGrow: string
  ): Promise<string> {
    update();
    // ruleid:typescript.typeorm.security.audit.raw-sql
    await getConnection()
      .createQueryBuilder("ddd")
      .update()
       //ERROR: match
      .set({
        firstName: "Timber",
        lastName: "Saw",
        age: () => "age + " + numberOfYearsToGrow,
      })
      .where("id = :id", { id: 1 })
      .execute();

    // ok:typescript.typeorm.security.audit.raw-sql
    await getConnection()
      .createQueryBuilder()
      .update()
       //ERROR: match
      .set({
        firstName: "Timber",
        lastName: "Saw",
        age: () => "age + 1",
      })
      .where("id = :id", { id: 1 })
      .execute();

    return "hello";
  }
}
