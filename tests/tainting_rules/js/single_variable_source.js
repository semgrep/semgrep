function($scope, $sce) {
   value = $scope.html
   //ruleid: detect-angular-trust-as-method
   $sce.trustAs($sce.HTML, value);
}
