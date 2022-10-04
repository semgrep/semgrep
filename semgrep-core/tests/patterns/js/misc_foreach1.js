export function findEvent(entityName: string, events: any[]): any {
  //ERROR: match
  for (const event of events) {
    if (event.entity_name === entityName) {
      return event.color;
    }
  }
  return {};
}