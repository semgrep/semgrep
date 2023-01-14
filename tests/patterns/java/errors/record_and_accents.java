public class Usecase {
    public Usecase() {
    }

    public void exécuter(Commande commande) {
    }

    public static record Commande(String param) {
        public static Commande aPartirDe(Chose chose) {
            return new Commande(chose.enParam());
        }
    }
}
