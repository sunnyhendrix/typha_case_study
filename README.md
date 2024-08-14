Hendrix SL. Using ChatGPT to write code that analyzes and produces vizualizations of aquatic plant data in R.

Overview

Aquatic plants have adaptations which enable them to endure variable water depths, but their ability to survive periods of complete flooding is poorly known. Extreme weather events brought on by climate change may cause more frequent periods of deep flooding (Milly et al. 2002). Wetland managers need to know how increased flood magnitudes impact plant functionality, biodiversity, and community composition. I examined the response of the ubiquitous freshwater marsh macrophyte, Typha domingensis, to deep flooding in an experimental tank for durations between 2 and 6 weeks. Numerous studies have focused on the effects of flood depth and duration on Typha, but only one study evaluated survival under an extended period of complete submergence, where water level increase coincided with leaf elongation rate (Vivian et al. 2014). The data collected from each potted plant used in the flood experiment included the maximum height, number of shoots, number of flowers, weeks submerged and whether or not the plant survived inundation. Chatbots have been proven to be adept at handling small, discrete programming tasks in multiple coding languages (Merow et al. 2023, Perkel 2023). I used ChatGPT to develop a custom script in R for data science to analyze and visualize the data, which was done using ggplot2, a core package of the tidyverse included in R (Wickham et al. 2023). Without specifying which statistical tests to use, ChatGPT provided R code, with errors, that performed
Chi-squared tests and logistic regressions. While ChatGPT did not provide  

References:

1. Grace JB. 1989. Effects of water depth on Typha latifolia and Typha domingensis. Am J Bot 76(5):762. https://doi.org/10.2307/2444423
2. Merow C, Serra-Diaz, JM, Enquist, BJ, & Wilson AM. 2023. AI chatbots can boost scientific coding. Nature Ecology & Evolution. https://doi.org/10.1038/s41559-023-02063-3 
3. Milly, PCD, Wetherald, RT, Dunne, KA, Delworth, TL. 2002. Increasing risk of great floods in a changing climate. Nature 415:514-517. https://doi.org/10.1038/415514a 
4. Perkel JM. 2023. Six tips for better coding with ChatGPT. Nature. https://doi.org/10.1038/d41586-023-01833-0
5. Vivian, LM, Godfree, RC, Colloff, MJ, Mayence, CE & Marshall, DJ. 2014. Wetland plant growth under contrasting water regimes associated with river regulation and drought: implications for environmental water management. 6. Plant Ecology 215(9), 997-1011. https://doi.org/10.1007/s11258-014-0357-4  
7. Wickham H, Çetinkaya-Rundel M, Grolemund G. 2023. R for Data Science: Import, Tidy, Transform, Visualize, and Model Data 2nd Edition. https://r4ds.hadley.nz/
