// in modern editors you can not see it, but between { and BenefitsDAO below
// there is a special unicode char: the non-breaking-space
const { BenefitsDAO } = require("../data/benefits-dao");

function BenefitsHandler (db) {
    "use strict";

    const benefitsDAO = new BenefitsDAO(db);

    this.displayBenefits = (req, res, next) => {

        benefitsDAO.getAllNonAdminUsers((error, users) => {

            if (error) return next(error);

            return res.render("benefits", {
                users,
                user: {
                    isAdmin: true
                }
            });
        });
    };

    this.updateBenefits = (req, res, next) => {
        const { userId, benefitStartDate } = req.body;

        benefitsDAO.updateBenefits(userId, benefitStartDate, (error) => {

            if (error) return next(error);

            benefitsDAO.getAllNonAdminUsers((error, users) => {
                if (error) return next(error);

                const data = {
                    users,
                    user: {
                        isAdmin: true
                    },
                    updateSuccess: true
                };

                return res.render("benefits", data);
            });
        });
    };
}

module.exports = BenefitsHandler;
