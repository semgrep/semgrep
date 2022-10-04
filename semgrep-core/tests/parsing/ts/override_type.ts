import { Field, ObjectType, ReturnTypeFuncValue } from '@nestjs/graphql';

import { SubSectionLabParameter } from '../../../../enum/medical_data/SubSection';
import { LabResultNumberParameterData } from '../LabResultNumberParameterData';

@ObjectType()
export class LabResultsFerritinData extends LabResultNumberParameterData {
  @Field((): ReturnTypeFuncValue => SubSectionLabParameter)
  override type: SubSectionLabParameter =
	SubSectionLabParameter.PARAMETER_FERRITIN;
    
