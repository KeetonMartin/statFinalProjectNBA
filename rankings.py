#get rankings by date

import pandas as pd


rankings = pd.read_csv("ranking.csv")
games = pd.read_csv("games.csv")

print("type: ", type(rankings["STANDINGSDATE"][0]))

goodSeasonsOfGames = games[((games["SEASON"] == 2017) | (games["SEASON"] == 2018))]
goodSeasonsOfRankings = rankings[(((rankings["SEASON_ID"] >= 22017) & (rankings["SEASON_ID"] <= 22018)) | ((rankings["SEASON_ID"] >= 12017) & (rankings["SEASON_ID"] <= 12018)))]

winPCT_home = []
winPCT_away = []

# print(len(rankings[0]))

numRows = len(goodSeasonsOfGames.index)
statusTrackerCounter = 0

# quitCounter = 0

for index, row in goodSeasonsOfGames.iterrows():
	statusTrackerCounter +=1
	# quitCounter +=1
	print("status report: ", statusTrackerCounter/numRows)

	HOME_TEAM_ID = row["HOME_TEAM_ID"]
	AWAY_TEAM_ID = row["VISITOR_TEAM_ID"]
	GAME_DATE = row["GAME_DATE_EST"]

	numTeamsFound = 0

	for index, row in goodSeasonsOfRankings.iterrows():
		currDate = row["STANDINGSDATE"]
		currTeam = row["TEAM_ID"]

		if currTeam == AWAY_TEAM_ID and currDate == GAME_DATE:
			winPCT_away.append(row["W_PCT"])
			numTeamsFound +=1

		if currTeam == HOME_TEAM_ID and currDate == GAME_DATE:
			winPCT_home.append(row["W_PCT"])
			numTeamsFound +=1

		if numTeamsFound >= 2:
			break 

	# if quitCounter >= 20:
	# 	break

print("WinPCT home len:", len(winPCT_home), " and first few: ", winPCT_home[0:6])
print("WinPCT away len:", len(winPCT_away), " and first few: ", winPCT_away[0:6])

outputDictionary = {'winPCT_home':winPCT_home, "winPCT_away":winPCT_away}
outputDf = pd.DataFrame(outputDictionary)
outputDf.to_csv("testOfWinPCTs.csv")


