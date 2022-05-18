export class HexVersionSecret extends HexVersionGQLEntity {
  inFileSpec: FileSpecAcknowledgements = "not included in export file format";
  @Field(() => HexVersionSecretIdScalar)
  override id!: HexVersionSecretId;
}
