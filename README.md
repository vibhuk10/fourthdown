# 4th Down Conversion Decision Tool
  By Vibhu Krishnan

  When should teams go for it on 4th down? 4th downs are one of the greatest factors in winning football games. Teams don’t go for it often and usually only go for it     when they are in a bad position to win the game. Data from the past 10 years shows that teams only go for it on 12.9% of 4th downs and only 7% of the time in the first   3 quarters. By using analytics and data science, I have created a tool that gives probabilities of winning for each option a team has on 4th down so coaches can be       more informed when making decisions on 4th down.

  My tool uses play by play data from the 2009-2019 NFL seasons. First, the user (the coach) inputs values based on the situation that they are in. They can input         quarter, time, score, yards to go, and the yardline so the tool can be accurate as possible and can accurately represent the situation the team is facing. Then, the     tool uses these inputs to find a probability of making a field goal, converting a fourth down, or punting the ball based on past data. Next, it finds the probability     of winning the game based on the different possible outcomes of a field goal, fourth down, or punt. It finds all the past games where a team faced a similar situation   and finds a win probability based on those games. It finds a win probability for all the outcomes like making a field goal vs. missing a field goal, converting a         fourth down vs. turning the ball over, and punting the ball vs. the punt being blocked. The final step is to use the concept Expected Probability to compare the         probability of winning for failing or converting each of the different play options and come up with an overall win probability for each option. Expected Win             Probability = (WP of Success * Chance of Success) + (WP of Failure * Chance of Failure). 
  
  Check out the comversion Tool here: https://vibhuk10.shinyapps.io/fourthdown/
  
  Read the full article with methodology, data, and results here: https://medium.com/@vibhuk10/powerful-use-of-data-science-on-4th-downs-to-help-win-football-games-       5bfc6747fe7a

