# 1.1 데이터 준비하기  

# 기생충 기사 댓글 불러오기  
#install.packages("dplyr")
library(readr)  
library(dplyr)  
raw_news_comment <- read_csv(file.choose()) %>%   mutate(id = row_number())  

# ------------------------------------------------------------------------- 
#install.packages("stringr")
#install.packages("textclean")

library(stringr)  
library(textclean)  

# 기본적인전처리(특수문자 제거, 3단어 이상의 문장만 추출출)
news_comment <- raw_news_comment %>%  
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "),  #특수문자 제거하는 정규식
         reply = str_squish(reply)) %>%  #빈칸이 많은 경우 제거
  
  # 중복 댓글 제거  
  distinct(reply, .keep_all = T) %>%  
  
  # 짧은 문서 제거 - 3단어 이상 추출  
  filter(str_count(reply, boundary("word")) >= 3)  


## KoNLP 설치----각자 컴퓨터 및 SW버전 등 상황에 따라 약간씩 다를 수 있음.  
#install.packages("multilinguer")  
library(multilinguer)  
#install_jdk()  
#install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"),   type = "binary")  
#install.packages("remotes")  
#remotes::install_github("haven-jeon/KoNLP",  
#                        upgrade = "never",  
#                        INSTALL_opts = c("--no-multiarch"),   force = TRUE)  
library(KoNLP)  
useNIADic() 

# -------------------------------------------------------------------------  
#install.packages("tidytext")
library(tidytext)  
library(KoNLP)  

# 명사 추출 (문장 단위의 DB를 단어 단위로 추출출)
comment <- news_comment %>%  
  unnest_tokens(input = reply,  
                output = word,  
                token = extractNoun,  
                drop = F) %>%  
  filter(str_count(word) > 1) %>%  
  
  # 댓글 내 중복 단어 제거  
  group_by(id) %>%  
  distinct(word, .keep_all = T) %>%  
  ungroup() %>%  
  select(id, word)  

# "%>%" 에러가 뜨면 위의 전처리에 사용했던 library들 다시 실행해주기

comment  

# ------빈도가 너무 높은 단어 제거하기기----------------------------------- 
count_word <- comment %>%  
add_count(word) %>%  
  filter(n <= 200) %>%  
  select(-n)  

# -------------------------------------------------------------------------  
# 불용어, 유의어 확인하기  
count_word %>%  
  count(word, sort = T) %>%  
  print(n = 200)  

# -------------------------------------------------------------------------
#데이터 처리자의 주관적인 의견이 반영되어야 하는 코드
#불필요하게 느껴지는 단어들 목록 만들어서 제거하기 (1,2번 항목목)

# 1.불용어 목록 만들기  
stopword <- c("들이", "하다", "하게", "하면", "해서", "이번", "하네",   "해요", "이것", "니들", "하기", "하지", "한거", "해주",   "그것", "어디", "여기", "까지", "이거", "하신", "만큼")  

# -------------------------------------------------------------------------  
# 2.불용어, 유의어 처리하기  
count_word <- count_word %>%  
  filter(!word %in% stopword) %>%  
  mutate(word = recode(word,  
                       "자랑스럽습니" = "자랑", 
                       "자랑스럽" = "자랑",  
                       "자한" = "자유한국당",  
                       "문재" = "문재인",  
                       "한국의" = "한국",  
                       "그네" = "박근혜",  
                       "추카" = "축하",  
                       "정경" = "정경심",  
                       "방탄" = "방탄소년단"))  

# -------------------------------------------------------------------------  

#######추가적으로 불용어 목록을 이어갈 수 있다는 것을 보여주기 위한 코드드
# tibble 불용어 목록 만들기  
stopword <- tibble(word = c("들이", "하다", "하게", "하면", "해서", "이번", "하네",   "해요", "이것", "니들", "하기", "하지", "한거", "해주",   "그것", "어디", "여기", "까지", "이거", "하신", "만큼"))  
# 불용어 목록 저장하기  
library(readr)  
write_csv(stopword, "stopword.csv")  
# 불용어 목록 불러오기  
stopword <- read_csv("stopword.csv")  
# 불용어 제거하기  
count_word <- count_word %>%  
 filter(!word %in% stopword$word)  

# -------------------------------------------------------------------------  
count_word <- count_word %>%  
  anti_join(stopword, by = "word") 

count_word

############################
########토픽 모델링########
# 1.2 모델링하기  
# 문서별 단어 빈도 구하기  
count_word_doc <- count_word %>%  
 count(id, word, sort = T)  
count_word_doc  

# -------------------------------------------------------------------------  
#install.packages("tm")  
library(tm) 
# DTM 만들기  
dtm_comment <- count_word_doc %>%  
 cast_dtm(document = id, term = word, value = n)  

dtm_comment  

as.matrix(dtm_comment)[1:8, 1:8] #가로 세로 8줄씩 노출하라는 명령어

# -------------------------------------------------------------------------  
#install.packages("topicmodels")  
library(topicmodels)  

# 토픽 모델 만들기  
lda_model <- LDA(dtm_comment,  
                k = 8,  
                method = "Gibbs",  
                control = list(seed = 1234))  #seed 지정 : 난수를 제어하여 언제 DB를 조회하더라도 동일하도록 설정 
lda_model  
# 모델 내용 확인  
glimpse(lda_model)  


# 토픽 속 단어들 살펴보기(=beta 값 살펴보기)
# 문서들을 토픽따라 분류해보기(=gamma 살펴보기)
# 2.1 토픽 속 주요 단어 살펴보기  
#install.packages("reshape2")
library(reshape2)
term_topic <- tidy(lda_model, matrix = "beta")  
term_topic[1:20,] #1~20개까지 노출하라는 의미

#beta 값 == 해당 토픽(인덱스 번호)에 객체가 나올 확률


# -------------------------------------------------------------------------  
# 토픽 별 단어 수  = 토픽 별 사용된 단어의 횟수
term_topic %>%  
 count(topic)  

# 토픽1의 beta 합계  = 토픽의 모든 단어를 합쳤을 때(모든 경우의 수(확률)을 합하므로 1이 된다.)
term_topic %>%  
 filter(topic == 1) %>%  
 summarise(sum_beta = sum(beta))  

# -------------------------------------------------------------------------  
# 관련 단어에 대한 연관성을 확률로써 확인할 수 있는 방법
term_topic %>%  
filter(term == "작품상") #조회 하려는 정보를 입력해서 확률을 검색하기

# -------------------------------------------------------------------------  
#각 토픽별 beta가 높은 순서대로 표기 (각 토픽별로 관심사나 내용을 유추할 수 있다.)
term_topic %>%  
filter(topic == 6) %>%  
 arrange(-beta)  
terms(lda_model, 20) %>%  
 data.frame()  

# -------------------------------------------------------------------------  
# 토픽별 beta 상위 10개 단어 추출  -> 그래프를 그리기 위해 상위10개 추출
top_term_topic <- term_topic %>%  
 group_by(topic) %>%  
 slice_max(beta, n = 10)  

# ------------------------------------------------------------------------- 
#그래프로 그리기 (시각화하여 beta마다의 확률을 따질 수 있다. -> 주요 내용 파악)
#install.packages("scales")  
library(scales)  
library(ggplot2)  

ggplot(top_term_topic,  
      aes(x = reorder_within(term, beta, topic),  
          y = beta,  
          fill = factor(topic))) +  
 geom_col(show.legend = F) +  
 facet_wrap(~ topic, scales = "free", ncol = 4) +  
 coord_flip() +  
 scale_x_reordered() +  
 scale_y_continuous(n.breaks = 4,  
                    labels = number_format(accuracy = .01)) +   labs(x = NULL) +  
 theme(text = element_text(family = "nanumgothic"))  


#--------------------------------------------------------------------------
# 2.2 문서들을 토픽 따라 분류해 보기  

doc_topic <- tidy(lda_model, matrix = "gamma")
doc_topic  
# -------------------------------------------------------------------------  
doc_topic %>%  
count(topic)  

# 문서 1의 gamma 합계 (모든 gamma의 값을 합치면 1이 된다.)
doc_topic %>%  
 filter(document == 1) %>%  
 summarise(sum_gamma = sum(gamma)) 

# -------------------------------------------------------------------------  
# 문서별로 확률이 가장 높은 토픽 추출  
doc_class <- doc_topic %>%  
 group_by(document) %>%  
 slice_max(gamma, n = 1)  
doc_class  


# ------------------------------------------------------------------------- 
#---------------원문에 토픽번호 부여하기-----------------------------------
# integer로 변환  
doc_class$document <- as.integer(doc_class$document)  

# 원문에 토픽 번호 부여  
news_comment_topic <- raw_news_comment %>%   left_join(doc_class, by = c("id" = "document"))  

# -------------------------------------------------------------------------  
# 결합 확인  
news_comment_topic %>%  
 select(id, topic)  

news_comment_topic %>%  
 count(topic)  

news_comment_topic <- news_comment_topic %>%   
  na.omit()  

#NA 처리가 된 데이터는 정제 과정에서 삭제된(배제된) 데이터를 의미한다.


# -------------------------------------------------------------------------  
#-----문서별로 빈도가 가장 높은 한 개의 토픽만 배정하기 -------------------
doc_topic %>%  
group_by(document) %>%  
 slice_max(gamma, n = 1) %>%  
 count(document) %>%  
 filter(n >= 2)  
# -------------------------------------------------------------------------  
set.seed(1234)  
doc_class_unique <- doc_topic %>%  
 group_by(document) %>%  
 slice_max(gamma, n = 1) %>% 
 slice_sample(n = 1)  
doc_class_unique  


# -------------------------------------------------------------------------  
# 문서 빈도 구하기  
doc_class_unique %>%  
 count(document, sort = T) 





# ------------------------------------------------------------------------- 
# 토픽별로 중요 단어 뽑아내기
top_terms <- term_topic %>%  
group_by(topic) %>%  
 slice_max(beta, n = 6, with_ties = F) %>%  
 summarise(term = paste(term, collapse = ", "))  
top_terms  
# -------------------------------------------------------------------------  
#토픽별 문서의 수 연산
count_topic <- news_comment_topic %>%  
count(topic)  
count_topic  
# -------------------------------------------------------------------------  
count_topic_word <- count_topic %>%  
left_join(top_terms, by = "topic") %>%  
 mutate(topic_name = paste("Topic", topic))  
count_topic_word  
# -------------------------------------------------------------------------  
# 그래프 그리기
library(scales)  
library(ggplot2)  
ggplot(count_topic_word,  
      aes(x = reorder(topic_name, n),  
         y = n,  
         fill = topic_name)) +  
        geom_col(show.legend = F) +  
        coord_flip() +  
  
        geom_text(aes(label = n) , # 문서 빈도 표시   
                  hjust = -0.2) + # 막대 밖에 표시  
  
                  geom_text(aes(label = term), # 주요 단어 표시 
                            hjust = 1.03, # 막대 안에 표시   col = "white", # 색깔  
                            fontface = "bold", # 두껍게  
                            family = "nanumgothic") + # 폰트 
                    
                    scale_y_continuous(expand = c(0, 0), # y축-막대 간격 줄이기   limits = c(0, 820)) + # y축 범위  
                                       labs(x = NULL) )
                                 


# 2.3 토픽 이름 지어보기  
#gamma 값에 따라 문서 살펴보기
comment_topic <- news_comment_topic %>%  
 mutate(reply = str_squish(replace_html(reply))) %>%  
 arrange(-gamma)  

comment_topic %>%  
 select(gamma, reply)  

# -------------------------------------------------------------------------  
# 토픽 1 내용 살펴보기  (토픽과 단어가 포함된 DB원본 파악하기)
comment_topic %>%  
 filter(topic == 1 & str_detect(reply, "작품")) %>%  
 head(50) %>%  
 pull(reply)  

comment_topic %>%  
 filter(topic == 1 & str_detect(reply, "진심")) %>%  
 head(50) %>%  
 pull(reply)  

comment_topic %>%  
 filter(topic == 1 & str_detect(reply, "정치")) %>%  
 head(50) %>%  
 pull(reply)  

# -------------------------------------------------------------------------  
# 토픽 이름 목록 만들기  
name_topic <- tibble(topic = 1:8,  
                    name = c("1. 작품상 수상 축하, 정치적 댓글 비판",   
                             "2. 수상 축하, 시상식 감상",   
                             "3. 조국 가족, 정치적 해석",   
                             "4. 새 역사 쓴 세계적인 영화",   
                             "5. 자랑스럽고 감사한 마음", 
                             "6. 놀라운 4관왕 수상",   
                             "7. 문화계 블랙리스트, 보수 정당 비판",   
                             "8. 한국의 세계적 위상"))  

# 토픽 이름 결합하기  (토픽1에 "작품" 단어가 나올 확률)
top_term_topic_name <- top_term_topic %>%  
 left_join(name_topic, name_topic, by = "topic")  
top_term_topic_name  

# 막대 그래프 만들기  
ggplot(top_term_topic_name,  
      aes(x = reorder_within(term, beta, name),  
          y = beta,  
          fill = factor(topic))) +  
 geom_col(show.legend = F) +  
 facet_wrap(~ name, scales = "free", ncol = 2) +  
 coord_flip() +  
 scale_x_reordered() +  
 labs(title = "영화 기생충 아카데미상 수상 기사 댓글 토픽",   subtitle = "토픽별 주요 단어 Top 10",  
      x = NULL, y = NULL) +  
 theme_minimal() +  
 theme(text = element_text(family = "nanumgothic"),  
       title = element_text(size = 12),  
       axis.text.x = element_blank(),  
       axis.ticks.x = element_blank())  



# 2.4 최적의 토픽 수 정해보기  
#install.packages("ldatuning")  
library(ldatuning)  

models <- FindTopicsNumber(dtm = dtm_comment,  
                          topics = 2:20,   #토픽이 2개일 때부터 20개까지 check하도록 자동 연산 명령 부여
                          return_models = T,  
                          control = list(seed = 1234)) 

models %>%  
 select(topics, Griffiths2004)  #Griffiths값은 크면 클수록 좋다.

FindTopicsNumber_plot(models) #여려가지 측면을 고려해서 가장 적합한 토픽의 수를 정할 수 있는 가이드라인을 제시하는 그래프 
                                 