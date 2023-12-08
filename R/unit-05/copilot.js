function calculateDaysBetweenDates(begin, end) {
    var beginDate = new Date(begin);
    var endDate = new Date(end);
    var millisecondsPerDay = 1000 * 60 * 60 * 24;
    var millisBetween = endDate.getTime() - beginDate.getTime();
    var days = millisBetween / millisecondsPerDay;
    return Math.floor(days);
}

function calculateAge(birthDate) {
    var birth = new Date(birthDate);
    var now = new Date();
    var age = now.getFullYear() - birth.getFullYear();
    var m = now.getMonth() - birth.getMonth();
    if (m < 0 || (m === 0 && now.getDate() < birth.getDate())) {
        age--;
    }
    return age;
}
