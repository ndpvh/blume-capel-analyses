# Validation of Attitudinal Blume-Capel model

Analyses targeted at a preliminary validation of the Attitudinal Blume-Capel model. 

This project contains the following structure:

- `scripts`: All scripts containing the analyses;
- `data`: The datasets used for the analyses;
- `results`: All results for the analyses on each (relevant) dataset;
- `figures`: Visualizations of the results and data;
- `models`: Models that are called when performing the analyses.

In what follows, we will describe the data, the analyses, and where to find the relevant code.



## Data

Within this project, we use three datasets collected within our lab group. While each has a different goal, all are relevant for making our point regarding neutrality in attitudes.

The `DEKKER` dataset was collected to investigate political attitudes in Dutch youngsters with and without a migration background. Participants had to indicate whether they agreed/disagreed with a particular statement (7-point Likert scale), how important that statement is for them (4-point Likert scale), and, if they choose the middle option, why they did so (1: "No opinion", 2: "Agreed and disagreed", 3: "Don't understand the question"). The different statements are the following:

- `s_klimaat`: Climate-related issues are exaggerated (Het klimaatprobleem wordt overdreven);
- `s_vlees`: The Dutch should eat less meat (Nederlanders moeten minder vlees eten);
- `s_immigratie`: Immigrants have to adapt to Dutch culture (Immigranten moeten zich aanpassen aan de Nederlandse cultuur);
- `s_vluchtelingen`: The Netherlands has to stop taking in refugees (Nederland moet stoppen met vluchtelingen opvangen);
- `s_belasting`: Rich people need to pay more taxes (Rijke mensen moeten meer belasting betalen);
- `s_israel`: Israel should withdraw from Gaza (Israël moet zich terugtrekken uit de Gazastrook);
- `s_gender`: Whether you're a boy or a girl is determined at birth (Of je een jongen of meisje bent, staat vast vanaf je geboorte);
- `s_leider`: Men are better fit as leaders than women (Mannen zijn geschikter als leider dan vrouwen);
- `s_kansen`: Rich parents have to contribute to the costs for school trips for children from a poorer background (Rijke ouders moeten meebetalen aan het schoolreisje voor kinderen van arme ouders); 
- `s_mobiel`: Mobile phones should be prohibited from the classroom (Mobiele telefoons moeten verboden zijn in de klas);
- `s_paarsevrijdag`: Every school should celebrate Purple Friday (Alle scholen moeten Paarse Vrijdag vieren);
- `s_voorlichting`: Sex education does not belong in a school's curriculum (Seksuele voorlichting hoort niet op school);
- `s_toiletten`: At least half of the toilets at school should be gender neutral (Minstens de helft van de toiletten op school moet genderneutraal zijn);
- `s_roddel`: Students who have a secret gossip-account on social media should be punished (Leerlingen die een geheim roddelaccount hebben op sociale media, moeten gestraft worden);
- `s_stemmen`: Youth should be allowed to vote when they're older than 16 years old (Jongeren moeten vanaf 16 jaar mogen stemmen).

The `MILLER` dataset was collected as a second study in the context of a master's thesis. Participants were collected through Prolific and had to answer 4 attitude questions regarding a judge's sentencing in two separate cases (5-point Likert scale with 6th "don't know" option), indicate the certainty of their answer (1-100 scale), and, if they provided a mipoint response or a certainty lower than 50, they had to provide a reason for this (options reflecting caution, reluctance, or indifference). The attitude questions were the following:

- `1`: I have confidence in these judges;
- `2`: These judges passed their judgment about the suspect in an impartial manner;
- `3`: I feel that these judges came to a legally correct sentence;
- `4`: These judges weighed all circumstances appropriately when making the judgment.

In this study, the amount of information the participants received about the case was manipulated, so that they were either provided with full information (`condition = 1`) or with partial information (`condition = 2`).

The `PILOT` dataset was collected to investigate the mechanisms underlying midpoint responding, specifically targetting feelings of ambivalence (`condition = "conflict"`) and the integration of information (`condition = "integration"`). Participants were required to fill out a questionnaire asking for their attitudes towards economic topics and meat consumption (5-point Likert; `_pre`). Then, they either had to provide arguments for their position (`condition == "control"`), they had to come up with both pro and contra arguments for the statements (`condition == "conflict"`), or they had to integrate pro and contra arguments into a single attitude (`condition == "integration"`). Afterwards, they completed the same questionnaire again using the same rating scale (`_post`). The questions that were asked are the following:

- `hc1`:
- `hc2`:
- `hc3`:
- `costs1`:
- `costs2`:
- `costs3`:
- `tariff1`:
- `tariff2`:
- `tariff3`:
- `meat1`:
- `meat2`:
- `meat3`:
- `meat4`:
- `meat5`:



## Descriptives

For each of the datasets, we first derive some descriptive statistics of the participant's answers to the questions in the studies. These descriptives are the following:

- Mean of the response on the Likert scales of the question (`mean` suffixed with `response`);
- Standard deviation of these responses (`sd` suffixed with `response`);
- Range and some quantiles of the responses (`min`, `max`, `q25`, `q50`, `q75` suffixed with `response`);
- Frequency that each discrete response was chosen (`response_`);
- Frequency with which a particular neutral response was chosen (depending on the dataset in question, `indifferent`, `neutral`, `ambivalent`, `cautious`, `reluctant`, `no_understanding`);
- Mean, standard deviation, range, and some quantiles of the importance measures (`mean`, `sd`,... suffixed with `importance`);
- Number of missing values in responses or importance (`missing`, only relevant for `DEKKER`);
- Mean, standard deviation, range, and some quantiles of the certainty measures (`mean`, `sd`,... suffixed with `certainty`, only relevant for `MILLER`)

The code to run these descriptive statistics on the datasets can be found in `descriptives.R`.



## Reason for Midpoint Responding

For each dataset, we also investigated the reasons participants report for using the midpoint response. This comes down to an informal check of the frequency with which participants select a particular reason for choosing the midpoint, choosing between indifference, neutrality, or misunderstanding (`DEKKER`), between indifference, caution, or reluctance (`MILLER`), or between indifference, ambivalence, caution, reluctance, or neutrality (`PILOT`). Importantly, counting the frequencies is done per item in the questionnaire separately and is conditional on participants choosing the midpoint response.

As a follow-up, we furthermore investigated the relationship between the reason behind the midpoint response and measured importance of a particular topic. For this, we distinguish between a model-expected involved neutrality (ambivalence, caution, reluctance, neutral) and uninvolved neutrality (indifference), coding the former as `1` and the latter as `0`. After recoding the midpoint responses, we then computed a correlation between the recoded variable and the importance of the topic as measured within the datasets.

The described analyses can be found in `neutrality_responses.R`.



## Modality

Another relevant analysis is asking how the responses are distributed, and whether the midpoint represents an often selected option or not. To answer this question, we identified modes in the responses, that is response options that are higher in frequency than their immediate neighbors, representing often-selected response options. 

This analysis consisted of the following procedure. First, we identified the modes in the distributions based on observed differences in frequencies. Once identified, we used a nonparametric bootstrap procedure of the differences of each mode with its immediate neighbors, further determining whether 0 is a part of the resulting 95%CI of the differences. If not, then we flagged the difference between the frequency with which the two responses were selected as statistically significant. Next, we defined a mode as significant when it was significantly higher than at least one of its neighbors. Finally, we counted the number of significant modes per item per dataset and furthermore determined whether the midpoint response was part of these significant modes.

The code to run this analysis can be found in `response_distribution.R`. Note that this code might change to use the newly introduced `remode` package instead.



## Manipulating Midpoint Responding

In the `PILOT` dataset, we attempted to manipulate midpoint responding through the inclusion of an integration or conflict task and then measuring the change in the participants' attitudes. To investigate the success of our manipulation, we estimated the probability of participant's to change from a particular starting state $S(0)$ to another state $S(1)$ after completing the task with a conditional probability matrix, specifically:

\begin{equation}
    \Pi(S(1) | S(0)) = \begin{bmatrix}
        \pi_{s_1 | s_1} & \pi_{s_1 | s_2} & \cdots & \pi_{s_1 | s_n} \\
        \pi_{s_2 | s_1} & \pi_{s_2 | s_2} & \cdots & \pi_{s_2 | s_n} \\
        \vdots & \vdots & \ddots & \vdots \\
        \pi_{s_n | s_1} & \pi_{s_n | s_2} & \cdots & \pi_{s_n | s_n} 
    \end{bmatrix}
\end{equation}

where $n$ is the number of discrete states that participants can switch to. In our analyses, these represent indifference, reluctance, caution, neutrality, ambivalence, and valenced (pro/contra) answers. Note that all probabilities $\pi$ sum to $1$ for each column.

Estimation of these probabilities is performed within a Bayesian framework, using Stan for this purpose. Priors on the probabilities were set to be vague and to include 0 and 1 as possibilities, reflecting the possibility that some switches never or always occurred. To find out whether there were differences between conditions, we computed the differences in probabilities across conditions and examined whether 0 was part of the 95% credibility interval. If not, then the difference was flagged as significant.

The code for the model can be found in the script `conditional_probabilities.stan` in the folder `models`. The code to run the analyses can furthermore be found in the script `conditional_probabilities.R`.
