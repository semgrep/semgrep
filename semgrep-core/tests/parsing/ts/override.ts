import { HexVersionSecretId, Role, SecretId } from "@hex/common";
import { GraphQLScalarType } from "graphql";
import { Field, ObjectType } from "type-graphql";
import { Column, Entity, Index, ManyToOne } from "typeorm";

import {
  AuthDirective,
  FileSpecAcknowledgements,
  HexVersionGQLEntity,
  IgnoredDuplicateFields,
  Secret,
  SecretIdScalar,
} from "../../internal";

export const HexVersionSecretIdScalar = new GraphQLScalarType({
  name: "HexVersionSecretId",
});

@AuthDirective({
  kind: "hasRole",
  role: Role.USER,
})
@ObjectType()
@Entity()
@Index(["hexVersion", "secret"], {
  unique: true,
  where: `"deletedDate" IS NULL`,
})
export class HexVersionSecret extends HexVersionGQLEntity {
  inFileSpec: FileSpecAcknowledgements = "not included in export file format";
  @Field(() => HexVersionSecretIdScalar)
  override id!: HexVersionSecretId;

  @ManyToOne(() => Secret, { nullable: false, onDelete: "CASCADE" })
  secret!: Promise<Secret>;

  @Column("uuid", { nullable: false })
  @Field(() => SecretIdScalar)
  secretId!: SecretId;
}

export type HexVersionSecretIgnoredDuplicateFields =
    IgnoredDuplicateFields<HexVersionSecret>;
