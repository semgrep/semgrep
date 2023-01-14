type MakeUpdateCellContentsOperationArgs<
  ACTION_TYPE extends `UPDATE_${string}_CELL`,
  ENTITY_NAME extends `${string}Cell`,
  FIELD_NAME extends `${Uncapitalize<ENTITY_NAME>}Id`,
> = {
  type: ACTION_TYPE;
  entityName: ENTITY_NAME;
  fieldName: FIELD_NAME;
};
