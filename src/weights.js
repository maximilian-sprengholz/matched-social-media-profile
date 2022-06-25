// HOW-TO WRITE THE WEIGHTS.
//
// How important is that they have a match on a given feature?
//
// Each weight can be a number, a function, or an object.
//
// If function, it should have a return statement for each
// allowed values. This way a weird value will return undefined
// and will be caught by the SimilarityMatcher.
//
// If object, each key of the object is an allowed value, and its value
// is the weight.


module.exports = {
    
    // DEMO 1.
    birthdayDate: 35,
    birthyear: 15,
    birthmonth: 10,
    birthday: function(value, p1, p2) {
        if (p1.birthmonth === p2.birthmonth) return 10;
        return 5;
    },
    zodiac: 15,
    age: function(value, p1, p2) {
        if (p1.gender === p2.gender) return 15;
        return 12;
    },
    agegroup: function(value, p1, p2) {
        if (p1.gender === p2.gender) return 20;
        return 10;
    },
    initials: 15,
    gender: 15,
    eyes: function(value) {
        if (value === 'Blue') return 20;
        return 5;
    },
    language: function(value, p1, p2) {
        if (value.toLowerCase() !== 'english') return 18;
        return 2;
    },
    otherlanguage: function(value, p1, p2, common) {
        return common.length * 15;
    },
    totlanguage: function(value, p1, p2, common) {
        if (value === 1) return 5;
        if (value === 2) return 10;
        if (value < 5) return 18;
        return 30;
    },
    othereyes: 15,
    othergender: 30,
    righthanded: {
        Right: 5,
        Left: 20,
        Ambidextrous: 30
    },
    race: (function() {
        function raceWeight(value) {
            if (value === 'White') return 15;
            if (value === 'African American') return 18;
            if (value === 'Latino') return 18;
            if (value === 'Asian') return 18;
            if (value === 'American Indian') return 25;
            if (value === 'Native Hawaiian') return 25;
            if (value === 'Pacific Islander') return 30;
            if (value === 'Alaska Native') return 30;
        }
        return function(value, p1, p2, common) {
            var res;
            res = raceWeight(common[0]);
            if (common.length < 2) return res;
            res += raceWeight(common[1]);
            if (common.length < 3) return res;
            res += raceWeight(common[2]);
            if (common.length < 4) return res;
            res += raceWeight(common[3]);
            if (common.length < 5) return res;
            res += raceWeight(common[4]);
            if (common.length < 6) return res;
            res += raceWeight(common[5]);
            if (common.length < 7) return res;
            res += raceWeight(common[6]);
            if (common.length < 8) return res;
            return res + raceWeight(common[7]);
        };
    })(),
    // GOD.
    god: 30,
    // PLACE.
    currentstate: 10,
    currenttown: 20,
    currentzip: 40,
    currentrural: 15,
    grownup_us: function(value, p1, p2) {
        if (value === 'No') return 15;
        if (value === 'Yes') return 5;
    },
    homestate_us: 15,
    hometown_us: 25,
    homezip_us: 45,
    homerural_us: 10,
    homestate_foreign: 20,
    hometown_foreign: 30,
    homezip_foreign: 50,
    homerural_foreign: 10,
    samestate: 5,
    sametown: 20,
    samezip: 30,
    secondgen: function(value, p1, p2, common) {
        if (value === 'No') return 0;
        if (value === 'Parents') return 25;
        if (value === 'Grandparents') return 15;
    },
    secondgencountry: function(value, p1, p2, common) {
        return 30 * common.length;
    },

    // FAMILY.
    marital: 10,
    parentsdivorced: function(value, p1, p2, common) {
        switch(value) {
        case 'Yes':
            return 25;
        case 'No':
            return 13;
        case 'They never married':
            return 18;
        case 'I don\'t know them':
            return 35;
        }
    },
    children: function(value, p1, p2) {
        // The values are relatively low because, they just add to
        // the children bracket.
        if (value === 0) return 0;
        if (value < 3) return 10;
        if (value < 6) return 20;
        return 30;
    },
    childrenbracket: function(value, p1, p2) {
        let bracket = p1.childrenbracket;
        if (bracket === 'more than 4') return 35;
        if (bracket === '3-4') return 25;
        if (bracket === '1-2') return 15;
        if (bracket === 'none') return 5;
    },
    siblings: function(value, p1, p2) {
        // The values are relatively low because, they just add to
        // the siblings bracket.
        if (value === 0) return 5;
        if (value < 3) return 10;
        if (value < 6) return 20;
        return 30;
    },
    siblingsbracket: function(value, p1, p2) {
        if (value === 'more than 4') return 25;
        if (value === '3-4') return 15;
        if (value === '1-2') return 10;
        if (value === 'none') return 10;
    },
    military: {
        Yes: 25,
        No: 5
    },
    militarybranch: 35,
    education: 10,
    college: 35,
    gayfriends: 30,
    lossfriend: function(v) {
        return v === 'Yes' ? 60: 2;
    },
    caregiver: function(v) {
        return v === 'Yes' ? 50: 2;
    },
    pets: function(value, p1, p2, common) {
        return 20 * common.length;
    },
    otherpets: function(value, p1, p2, common) {
        return 30 * common.length;
    },
    // FINANCE.
    employment: 10,
    ownhouse: 15,
    owncar: 10,
    studentdebt: function(value) {
        if (value === 'Yes, and it is large') return 30;
        if (value === 'Yes, but it is manageable') return 20;
        if (value === 'No, I have paid it off') return 15;
        if (value === 'No, I never had it') return 8;
    },
    income: 30, // TODO: think about how to use all the info.
    incomebracket: function(value) {
        if (value === '<25k') return 30;
        if (value === '25k-40k') return 25;
        if (value === '40k-75k') return 15;
        if (value === '75k-120k') return 25;
        if (value === '>120k') return 30;
    },
    incomeclassPastDirection: function(value) {
        if (value !== 'Same') return 15;
        return 5;
    },
    incomeclassFutureDirection: function(value) {
        if (value !== 'Same') return 15;
        return 5;
    },
    // same for child, now, and future.
    incomeclasschild: function(value, p1, p2) {
        if (value === 'Elite') return 50;
        if (value === 'Bottom') return 40;
        if (value === 'Upper') return 30;
        if (value === 'Lower') return 25;
        if (value === 'Lower-Middle' ||
            value === 'Middle' ||
            value === 'Upper-Middle') return 15;
    },
    incomeclass: function(value, p1, p2) {
        if (value === 'Elite') return 50;
        if (value === 'Bottom') return 40;
        if (value === 'Upper') return 30;
        if (value === 'Lower') return 25;
        if (value === 'Lower-Middle' ||
            value === 'Middle' ||
            value === 'Upper-Middle') return 15;
    },
    incomeclassfuture: function(value, p1, p2) {
        if (value === 'Elite') return 50;
        if (value === 'Bottom') return 40;
        if (value === 'Upper') return 30;
        if (value === 'Lower') return 25;
        if (value === 'Lower-Middle' ||
            value === 'Middle' ||
            value === 'Upper-Middle') return 15;
    },
    // PERSONALITY.
    workorplay: 20,
    energetic: 20,
    competitive: 20,
    perfectionist: function(value) {
        return value === 'Yes' ? 30 : 15;
    },
    patient: 15,
    messy: function(value) {
        if (value === 'Yes, really messy') return 30;
        if (value === 'Yes, a bit') return 15;
        // No.
        return 20;
    },
    carebody: 15,
    confrontational: 20,
    // PERSONALITY 2.
    fascination: 25,
    fairies: function(value) {
        return value === 'Yes' ? 30 : 5;
    },
    // BEHAVIOR.
    snooze: 30,
    streetfurniture: {
        Yes: 30,
        No: 10
    },
    giveaway: 20,
    stoleglass: {
        Yes: 40,
        No: 10,
        'Not appropriate to ask': 10
    },
    foodback: 30,
    giftrecycle: {
        Yes: 30,
        No: 20
    },
    profanelanguage: {
        Never: 25,
        Occasionally: 10,
        Often: 20,
        Regularly: 30
    },
    readhoroscope: {
        Daily: 30,
        Weekly: 15,
        Occasionally: 8,
        Never: 10
    },
    // TASTE 1.
    color: 15,
    othercolor: 25,
    food: 20,
    otherfood: 30,
    spicyfood: function(value) {
        if (value === 'I don\'t like spicy food') return 12;
        if (value === 'Spicy, but not too much') return 10;
        if (value === 'Hot') return 15;
        if (value === 'Extremely Hot') return 22;
    },
    vegetarian: function(value) {
        return value === 'Yes' ? 25 : 8;
    },
    countriesvisited: function(value) {
        if (value === 'Zero') return 15;
        if (value === 'Between 1 and 2') return 10;
        if (value === 'Between 3 and 5') return 15;
        if (value === 'Between 6 and 10') return 20;
        if (value === 'More than 10') return 28;
    },
    vacation: 25,
    // THINGS YOU DO.
    socialmedia: function(value) {
        if (value === 'I am a very active user') return 20;
        if (value === 'I am a somewhat active user') return 15;
        if (value === 'I rarely use them') return 12;
        if (value === 'I never use them') return 25;
    },
    fashion: function(value, p1, p2, common) {
        if (value === 'A lot') return 20;
        if (value === 'Moderate') return 15;
        if (value === 'Not much') return 12;
    },
    smoke: function(value) {
        if (value === 'Yes') return 20;
        if (value === 'Yes, socially') return 20;
        if (value === 'No') return 20;
        if (value === 'No, I quit it') return 25;
    },
    sportdo: function(value, p1, p2, common) {
        return 20 * common.length;
    },
    othersportdo: 30,
    museums: function(value) {
        if (value === 'Yes, I love it') return 25;
        if (value === 'Yes, sometimes') return 12;
        if (value === 'No') return 8;
    },
    dance: function(value) {
        if (value === 'Yes, I love it') return 25;
        if (value === 'Yes, sometimes') return 12;
        if (value === 'No') return 8;
    },
    // TASTE 2.
    musiclisten: function(value, p1, p2) {
        if (value === 'No') return 15;
        return 5;
    },
    music: function(value, p1, p2, common) {
        return 20 * common.length;
    },
    othermusic: 20,
    bestmusician: 30,
    moviefan: function(value, p1, p2) {
        if (value === 'No') return 15;
        return 5;
    },
    movie: function(value, p1, p2, common) {
        return 20 * common.length;
    },
    othermovie: 20,
    bestmovie: 30,
    bestactor: 30,
    sportfan: function(value, p1, p2) {
        if (value === 'No') return 15;
        return 5;
    },
    sportfollow: 20,
    othersportfollow: 30,
    bestteam: 40,
    watchtv: function(value, p1, p2) {
        if (value === 'No') return 15;
        return 5;
    },
    tvshows: function(value, p1, p2, common) {
        return 20 * common.length;
    },
    readbooks: function(value, p1, p2) {
        if (value === 'Yes') return 15;
        return 10;
    },
    books: function(value, p1, p2, common) {
        return 25 * common.length;
    },
    playvideogames: function(value, p1, p2, common) {
        if (value === 'Yes') return 20;
        return 5;
    },
    videgames: function(value, p1, p2, common) {
        return common.length * 30;
    },
    followwebchannels: function(value, p1, p2, common) {
        if (value === 'Yes') return 15;
        return 5;
    },
    webchannels: function(value, p1, p2, common) {
        return common.length * 40;
    },
    docreative: function(value) {
        return value === 'Yes' ? 20 : 8;
    },
    creative: function(value, p1, p2, common) {
        return common.length * 22;
    },
    otherfun: function(value, p1, p2, common) {
        return 35 * common.length;
    }
};
