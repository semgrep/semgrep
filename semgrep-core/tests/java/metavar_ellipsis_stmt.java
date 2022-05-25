class BeanFactoryManagerBean {
    void makeFactoryManageEnterprise(boolean en) {
        if (en) {
            int x = 7;
            for (int y = 0; y < 23; ++y) {
                x++;
            }
            return;
        }
    }

    void beanItUp(boolean beanTime) {
        // ERROR: match
        if (beanTime) {
            System.out.print("beans");
        } else {
            System.out.print("contact your administrator");
        }
    }
}
