interface PermissionsResponse {
  gitUrl: string;
  commitHash: string;
  permissions: {
  [name: string]: PermissionEntry;
  };
};
