public interface AccessTokenEx {

    enum TokenTypeEnum {

        ACCESSTOKEN(0){
            @Override
            public String getTypeName(){
                return AccessTokenEx.ACCESS_TOKEN;
            }
        },
        REFRESHTOKEN(1){
            @Override
            public String getTypeName(){
                return AccessTokenEx.REFRESH_TOKEN;
            }
        };

        private int type;

        TokenTypeEnum(int type){
            this.type = type;
        }

        public abstract String getTypeName();

        public int getType() {
            return type;
        }
    }

    String ACCESS_TOKEN = "accesstoken";
    String REFRESH_TOKEN = "refreshtoken";

}
