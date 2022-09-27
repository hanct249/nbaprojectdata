clc, clear, close all;

% load eightNBAseasons.mat
load twentyThreeNBAseasons.mat

colormap('winter');

Team = VarName2;

nanInd = isnan(OffensiveRating);
OffensiveRating(nanInd) = [];
DefensiveRating(nanInd) = [];
PlayoffWins(nanInd) = [];
Team(nanInd) = [];
Season(nanInd) = [];

% OffensiveRating = [OffensiveRating; 116.2];
% DefensiveRating = [DefensiveRating; 110.0];
% Team = [Team; 'Utah Jazz'];
% PlayoffWins = [PlayoffWins; 2];


textSize = 14;
markerSize = 60;
markerWidth = 2;
specialMarker = 'gx';
specialTeam = 'Atlanta Hawks';

madePlayoffs = PlayoffWins > -10;
made2ndRd = PlayoffWins >= 4;
madeConfFin = PlayoffWins >= 8;
madeFinals = PlayoffWins >= 12;
wonFinals = PlayoffWins == 16;

missedPlayoffInds = find(PlayoffWins == -10);
outIn1stRd = PlayoffWins < 4 & PlayoffWins > -1;
outIn2ndRd = PlayoffWins < 8 & PlayoffWins > 3;
outIn3rdRd = PlayoffWins < 12 & PlayoffWins > 7;
lostFinals = PlayoffWins < 16 & PlayoffWins > 11;

specialInds = find(Team==specialTeam); % Finish this

figure
subplot(2,3,1)
scatter(OffensiveRating,DefensiveRating,markerSize,madePlayoffs,'linewidth',markerWidth);
hold on;
plot(OffensiveRating(specialInds),DefensiveRating(specialInds),specialMarker,'linewidth',markerWidth);
grid on;
xlabel('Offensive Rating','FontSize',textSize);
ylabel('Defensive Rating','FontSize',textSize);
title('Made Playoffs','FontSize',textSize);

subplot(2,3,2)
scatter(OffensiveRating,DefensiveRating,markerSize,made2ndRd,'linewidth',markerWidth);
hold on;
plot(OffensiveRating(specialInds),DefensiveRating(specialInds),specialMarker,'linewidth',markerWidth);
grid on;
xlabel('Offensive Rating','FontSize',textSize);
ylabel('Defensive Rating','FontSize',textSize);
title('Made Second Round','FontSize',textSize);

subplot(2,3,3)
scatter(OffensiveRating,DefensiveRating,markerSize,madeConfFin,'linewidth',markerWidth);
hold on;
plot(OffensiveRating(specialInds),DefensiveRating(specialInds),specialMarker,'linewidth',markerWidth);
grid on;
xlabel('Offensive Rating','FontSize',textSize);
ylabel('Defensive Rating','FontSize',textSize);
title('Made Conference Finals','FontSize',textSize);

subplot(2,3,4)
scatter(OffensiveRating,DefensiveRating,markerSize,madeFinals,'linewidth',markerWidth);
hold on;
plot(OffensiveRating(specialInds),DefensiveRating(specialInds),specialMarker,'linewidth',markerWidth);
grid on;
xlabel('Offensive Rating','FontSize',textSize);
ylabel('Defensive Rating','FontSize',textSize);
title('Made NBA Finals','FontSize',textSize);

subplot(2,3,5)
scatter(OffensiveRating,DefensiveRating,markerSize,wonFinals,'linewidth',markerWidth);
hold on;
plot(OffensiveRating(specialInds),DefensiveRating(specialInds),specialMarker,'linewidth',markerWidth);
grid on;
xlabel('Offensive Rating','FontSize',textSize);
ylabel('Defensive Rating','FontSize',textSize);
title('Won NBA Finals','FontSize',textSize);

figure
plot(OffensiveRating(missedPlayoffInds),DefensiveRating(missedPlayoffInds),'o','linewidth',markerWidth);
hold on;
plot(OffensiveRating(outIn1stRd),DefensiveRating(outIn1stRd),'o','linewidth',markerWidth);
plot(OffensiveRating(outIn2ndRd),DefensiveRating(outIn2ndRd),'o','linewidth',markerWidth);
plot(OffensiveRating(outIn3rdRd),DefensiveRating(outIn3rdRd),'o','linewidth',markerWidth);
plot(OffensiveRating(lostFinals),DefensiveRating(lostFinals),'o','linewidth',markerWidth);
plot(OffensiveRating(wonFinals),DefensiveRating(wonFinals),'o','linewidth',markerWidth);
plot(OffensiveRating(specialInds),DefensiveRating(specialInds),specialMarker,'linewidth',markerWidth);
legend('Missed Playoffs','Out in 1st Rd','Out in 2nd Rd','Lost Conf Finals','Lost NBA Finals','Won NBA Finals',specialTeam,'Location','northwest');


%% How long does a rebuild last?  Who's the best at it?  Does it work, i.e., when it's over, are you better?

% What is a rebuild? How to define with data? We can use wins, playoff
% success, etc., but these should be worse predictors than soft data.
% Hard decisions always throw away information.  Let's use a combination of
% OR and DR to define how good a team is.  We're going to normalize the
% data too.

% Time to get back to where you were, time to get back to

% normalize the data

adjusted_OR = zeros(size(OffensiveRating));
adjusted_DR = zeros(size(DefensiveRating));

for yr = 2000:2022
    [ind] = find(Season==yr);
    aveOR = mean(OffensiveRating(ind));
    aveDR = mean(DefensiveRating(ind));
    adjusted_OR(ind) = OffensiveRating(ind)/aveOR;
    adjusted_DR(ind) = DefensiveRating(ind)/aveDR;
end

belowAve = adjusted_DR > adjusted_OR; 
badStreaks = NaN*ones(5,30); % a place to keep track of bad streaks for each team
currentlyBadStreaks = NaN*ones(1,30); % a place to keep track of bad streaks that aren't over yet
initiallyBadStreaks = NaN*ones(1,30); % a place to keep track of bad streaks that were already going at the start of our dataset

for ii = 1:30
    streakCount = 1; % looking for first bad streak
    switch ii
        case 1
            teamName = 'Atlanta Hawks';
            teamName2 = '';
        case 2
            teamName = 'Boston Celtics';
            teamName2 = '';
        case 3 
            teamName = 'Brooklyn Nets';
            teamName2 = 'New Jersey Nets';
        case 4
            teamName = 'Charlotte Hornets';
            teamName2 = 'Charlotte Bobcats';
        case 5
            teamName = 'Chicago Bulls';
            teamName2 = '';
        case 6
            teamName = 'Cleveland Cavaliers';
            teamName2 = '';
        case 7
            teamName = 'Dallas Mavericks';
            teamName2 = '';
        case 8
            teamName = 'Denver Nuggets';
            teamName2 = '';
        case 9
            teamName = 'Detroit Pistons';
            teamName2 = '';
        case 10
            teamName = 'Golden State Warriors';
            teamName2 = '';
        case 11
            teamName = 'Houston Rockets';
            teamName2 = '';
        case 12
            teamName = 'Indiana Pacers';
            teamName2 = '';
        case 13
            teamName = 'Los Angeles Clippers';
            teamName2 = 'LA Clippers';
        case 14
            teamName = 'Los Angeles Lakers';
            teamName2 = '';
        case 15
            teamName = 'Memphis Grizzlies';
            teamName2 = 'Vancouver Grizzlies';
        case 16
            teamName = 'Miami Heat';
            teamName2 = '';
        case 17
            teamName = 'Milwaukee Bucks';
            teamName2 = '';
        case 18
            teamName = 'Minnesota Timberwolves';
            teamName2 = '';
        case 19
            teamName = 'New Orleans Pelicans';
            teamName2 = 'New Orleans Hornets';
        case 20
            teamName = 'New York Knicks';
            teamName2 = '';
        case 21
            teamName = 'Oklahoma City Thunder';
            teamName2 = 'Seattle SuperSonics';
        case 22
            teamName = 'Orlando Magic';
            teamName2 = '';
        case 23
            teamName = 'Philadelphia 76ers';
            teamName2 = '';
        case 24
            teamName = 'Phoenix Suns';
            teamName2 = '';
        case 25
            teamName = 'Portland Trail Blazers';
            teamName2 = '';
        case 26
            teamName = 'Sacramento Kings';
            teamName2 = '';
        case 27
            teamName = 'San Antonio Spurs';
            teamName2 = '';
        case 28
            teamName = 'Toronto Raptors';
            teamName2 = '';
        case 29
            teamName = 'Utah Jazz';
            teamName2 = '';
        case 30
            teamName = 'Washington Wizards';
            teamName2 = '';
        otherwise
            error('This shouldn''t happen.');
    end
    teamInd1 = find(Team==teamName);
    teamInd2 = find(Team==teamName2);
    teamInd = [teamInd1; teamInd2];
    if ii == 4 % This is the Charlotte Hornets, who started as the Charlotte Bobcats in 2005, and then became the Hornets again later.
        deleteThese = [];
        for kk = 1:length(teamInd1)
            if Season(teamInd1(kk)) < 2005 % can't count "Charlotte Hornet's" seasons prior to 2005 since this team moved to New Orleans
                deleteThese = [deleteThese; kk];
            end
        end
        teamInd(deleteThese) = [];
    elseif ii == 19 % This is the New Orleans Pelicans, who started as the Charlotte Hornets, but moved to New Orleans in 2002.  Let's put on those first three seasons from when they were in Charlotte.
        teamInd3 = find(Team=='Charlotte Hornets');
        for kk = 1:length(teamInd3)
            if Season(teamInd3(kk)) < 2003
                teamInd = [teamInd; teamInd3(kk)];
            end
        end
        teamInd4 = find(Team=='New Orleans/Oklahoma City Hornets'); % Add Katrina seasons that were played in OKC
        teamInd = [teamInd; teamInd4];
    elseif ii == 21 % This is OKC Thunder.  Some of the name data is goofy.
        teamInd3 = find(Team=='Oklahoma City Thunder ');
        teamInd = [teamInd; teamInd3];
    end
    if length(teamInd)~=23
        warning(['We didn''t find 23 seasons for the ' teamName]);
    end
    team_below_ave = belowAve(teamInd);
    [~,order] = sort(Season(teamInd));
    teamInd  = teamInd(order);
    team_below_ave = team_below_ave(order);
    firstGood = find(team_below_ave==0,1,'first'); % find the first good season for the team in our database
    if firstGood > 1
        initiallyBadStreaks(1,ii) = firstGood - 1;
    end
    state = 1; % looking for first bad season state
    currentStreak = 0; % length of bad streak
    for jj = firstGood+1:length(team_below_ave)
        if state==1 && team_below_ave(jj)
            state = 2; % found a bad season
            currentStreak = 1;
        elseif state==2 && team_below_ave(jj)
            currentStreak = currentStreak + 1; % still in bad streak, just add one to the streak
            if jj==length(team_below_ave)
                currentlyBadStreaks(1,ii) = currentStreak;
                disp([teamName ' are on a current streak of badness lasting ' num2str(currentStreak) ' seasons.']);
            end
        elseif state==2
            state = 1; % start looking again since this streak is over
            badStreaks(streakCount,ii) = currentStreak; % store the streak
            streakCount = streakCount + 1; % increase the streakCount
            currentStreak = 0; % reset the streak length
        else
            state = 1; % still looking
        end
    end
end

disp('Initial Bad Streaks');
disp([1:30; initiallyBadStreaks]);

disp('Complete Streaks in Dataset');
disp([1:30; badStreaks]);

disp('Current Bad Streaks');
disp([1:30; currentlyBadStreaks]);

disp('Average Complete Streak');
disp(nanmean(reshape(badStreaks,1,[])));

playoffInds = find(PlayoffWins > -10);
% missedPlayoffInds = find(PlayoffWins == -10);
outIn1stRdInds = find(PlayoffWins < 4 & PlayoffWins > -1);
outIn2ndRdInds = find(PlayoffWins < 8 & PlayoffWins > 3);
outIn3rdRdInds = find(PlayoffWins < 12 & PlayoffWins > 7);
lostFinalsInds = find(PlayoffWins < 16 & PlayoffWins > 11);
wonFinalsInds = find(PlayoffWins == 16);

disp([mean(adjusted_DR(missedPlayoffInds)), mean(adjusted_OR(missedPlayoffInds)), mean(adjusted_OR(missedPlayoffInds)-adjusted_DR(missedPlayoffInds));...
    mean(adjusted_DR(outIn1stRdInds)), mean(adjusted_OR(outIn1stRdInds)), mean(adjusted_OR(outIn1stRdInds)-adjusted_DR(outIn1stRdInds));...
    mean(adjusted_DR(outIn2ndRdInds)), mean(adjusted_OR(outIn2ndRdInds)), mean(adjusted_OR(outIn2ndRdInds)-adjusted_DR(outIn2ndRdInds));...
    mean(adjusted_DR(outIn3rdRdInds)), mean(adjusted_OR(outIn3rdRdInds)), mean(adjusted_OR(outIn3rdRdInds)-adjusted_DR(outIn3rdRdInds));...
    mean(adjusted_DR(lostFinalsInds)), mean(adjusted_OR(lostFinalsInds)), mean(adjusted_OR(lostFinalsInds)-adjusted_DR(lostFinalsInds));...
    mean(adjusted_DR(wonFinalsInds)), mean(adjusted_OR(wonFinalsInds)), mean(adjusted_OR(wonFinalsInds)-adjusted_DR(wonFinalsInds))]);

% Need to estimate p, the probability that a team finishes a rebuild.  If
% we assume that this happens with independent Bernoulli trials, then the
% distribution that governs how many additional years it would take to
% finish these ongoing streaks is geometric(p).

totalAttemptsToFinish = sum(nansum([initiallyBadStreaks; badStreaks; currentlyBadStreaks]));
numberOfFinishedStreaks = sum(sum(~isnan(badStreaks)));

p = numberOfFinishedStreaks/totalAttemptsToFinish;

mean_Geo_p = 1/p;

disp('Estimate of Total Length of Initial Bad Streaks');
disp([1:30; initiallyBadStreaks + mean_Geo_p]);

disp('Estimate of Total Length of Current Bad Streaks');
disp([1:30; currentlyBadStreaks + mean_Geo_p]);

disp('Estimated Length of Streaks - should be close to 4.175')
disp(nanmean(reshape([badStreaks; initiallyBadStreaks + mean_Geo_p; currentlyBadStreaks + mean_Geo_p],1,[])));

disp('Number of Seasons in Rebuild');
disp([1:30; nansum([initiallyBadStreaks; badStreaks; currentlyBadStreaks])]);
