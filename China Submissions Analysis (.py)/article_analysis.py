# %%
import pandas as pd
import numpy as np
import streamlit as st

# %%
file = "China Data.csv"
input_file = pd.read_csv(file)
journal_list = input_file.copy()
journal_list.drop(
    [
        "Online Fiscal Year",
        "#Submissions",
        "Papers Published",
        "Accepted Papers",
        "Rejected Papers",
        "Referrals",
        "Transfers",
        "Acceptance Rate",
    ],
    axis=1,
    inplace=True,
)
journal_list.drop_duplicates(["Journal Code"], keep="first", inplace=True)

# %%
subs_pivot = pd.pivot_table(
    input_file,
    values=["#Submissions"],
    index=["Journal Code"],
    columns=["Online Fiscal Year"],
    fill_value=0,
)
subs_pivot["Total Submissions"] = subs_pivot.sum(axis=1)
subs_pivot.sort_values(by=["Total Submissions"], ascending=False, inplace=True)

# %%
accepted_pivot = pd.pivot_table(
    input_file,
    values=["Accepted Papers"],
    index=["Journal Code"],
    columns=["Online Fiscal Year"],
    fill_value=0,
)
accepted_pivot["Total Accepted"] = accepted_pivot.sum(axis=1)
accepted_pivot.sort_values(by=["Total Accepted"], ascending=False, inplace=True)

# %%
subs_total = subs_pivot["Total Submissions"]
accepted_total = accepted_pivot["Total Accepted"]
journal_join = pd.merge(subs_total, accepted_total, on="Journal Code", how="inner")
journal_join["Total Rejected"] = (
    journal_join["Total Submissions"] - journal_join["Total Accepted"]
)
journal_join["Rejected Percentage"] = (
    journal_join["Total Rejected"] / journal_join["Total Submissions"]
)

# %%
ebf_target_list = journal_join[
    (
        journal_join["Total Submissions"]
        > journal_join["Total Submissions"].quantile(0.75)
    )
    & (journal_join["Total Rejected"] > journal_join["Total Rejected"].quantile(0.75))
]
ebf_target_list.round(2)
ebf_target_list.sort_values(by=["Total Rejected"], ascending=False)

# %%
rejected = "China Rejected Articles.csv"
rejected_articles = pd.read_csv(rejected)
rejected_articles = rejected_articles.drop_duplicates(
    ["Sending -Manuscript ID"], keep="first"
)
rejected_articles["Articles"] = 1
rejected_articles["Contributing?"] = np.where(
    (
        rejected_articles["Receiving -2-Year Citations"]
        > rejected_articles["Sending -Impact Factor"]
    ),
    True,
    False,
)
rejected_articles.rename(
    columns={"Sending -Product Code": "Journal Code"}, inplace=True
)
# %%
proprtion_rejected = pd.pivot_table(
    rejected_articles,
    values=("Contributing?", "Desk Rejected?"),
    index=("Journal Code"),
)
proprtion_rejected.sort_values(["Contributing?"], ascending=False, inplace=True)

# %%
temp_merge = pd.merge(
    ebf_target_list, proprtion_rejected, on="Journal Code", how="inner"
)
final_merge = pd.merge(journal_list, temp_merge, on="Journal Code", how="inner")

# %%
final_merge.sort_values(by="Contributing?", ascending=False)
final_merge.drop(
    ["Total Submissions", "Total Accepted", "Total Rejected"], axis=1, inplace=True
)
final_merge["Rejected Percentage"] = final_merge["Rejected Percentage"] * 100
final_merge["Contributing?"] = final_merge["Contributing?"] * 100
final_merge["Desk Rejected?"] = final_merge["Desk Rejected?"] * 100
final_merge.round(2)
final_merge = final_merge[
    [
        "Journal Code",
        "Journal",
        "JPM",
        "WOL Subject",
        "Ownership",
        "Rejected Percentage",
        "Contributing?",
        "Desk Rejected?",
    ]
]
final_merge.rename(
    columns={
        "Rejected Percentage": "%age Chinese Articles Rejected",
        "Contributing?": "%age of rejects with 2YrC > JIF",
        "Desk Rejected?": "%age Desk Rejections ",
    }
)


# %%
st.title("EBF Team Chinese Article Analysis")
st.write(
    "*All analysis is based on direct exports from Qlik: Submissions by Title (JEVA) and Cascade by Title (Submissions Dashboard). Data correct as of 14th May 2021*"
)
st.write(
    "Please bear in mind that what follows represents a proof of concept and is still rough around the edges. Any and all feedback is greatly appreciated."
)
st.write("---")
st.write(
    "As a reminder, the below list was pulled together by analysing all titles within the EBF portfolio, and selecting those that were in the 75th percentile or above for both submissions and rejections for content from Chinese First Authors."
)
st.write("You can expand the list using the arrows to the top right of the dataframe.")
st.write(ebf_target_list)
st.write("---")
st.write(
    "As disuccsed we then pulled a list of rejected articles from all titles within the portfolio with Chinese first authors, then cross-referenced that list the target list defined above."
)
st.write(
    "To explain the dataframe below. The 'Contributing? column represents a completed comparison showing the average number of articles rejected by our titles, represented as a percentage, that go on to be published by competitor titles and receive enough 2-year citations to have been citationally valuable to our source title. "
)
st.write(
    "The 'Desk Rejected?' column then represents as a percentage the number of these Chinese first author papers that were rejected w/o review. An important note here is that this doesn't take into account the editorial practices that underly the S1M data. For example the three BSE titles on this list display as 100% but this is because they handle the review of papers offline."
)
st.write("---")
st.write(final_merge)
st.write("---")
st.write(
    "To aid in interpretation of this dataframe, a journal that has a high value in both the 'Contributing?' and 'Desk Rejected' column could potentially benefit from taking a more developmental approach to some of these Chinese articles, rather than passing them over to competitor titles."
)
