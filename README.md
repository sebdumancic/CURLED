# Learning Relational Latent Representation with Clustering

An implementation of the approach introduced in the paper `Clustering-based Unsupervised Relational Representation Learning with an Explicit Distributed Representation`, check the original paper for details. Still under development.

## Installation

+ get [SBT](http://www.scala-sbt.org/)
+ clone this repository
+ download the [ReCeNT](https://bitbucket.org/sdumancic/relationalclusteringoverneighbourhoodtrees) and follow the instruction how to compile it
+ add the jar in the `lib` folder of this project
+ position in the root folder of this project and
  ++ build a jar with dependencies `sbt assembly`
  ++ build a jar without dependencies `sbt package`

## Usage

### Arguments

```
Usage: LearnNewRepresentation.jar [OPTIONS]

OPTIONS

--db knowledgeBase                                     database(s) with data to cluster (May be specified multiple times.)                                                   
--declarations file path                               file containing declarations of predicates

--domain file file 		                               header for the knowledge base(s); specification of logical predicates

--algorithm [Spectral|Hierarchical]                    clustering algorithm to use

--bagCombination [union|intersection]                  bag combination method

--bagSimilarity [chiSquared|maximum|minimum|union]     bag similarity measure

--clusterValidation silhouette|intraClusterSimilarity  clustering validation method

--depth n                                              depth of the neighbourhood tree
                                                     
--kDomain comma-separated list of domain:numClusters   number of clusters per domain (if precise number needed, otherwise leave it to the clustering selection)
                                                       
--linkage [average|complete|ward]                      linkage for hierarchical clustering (use complete for now!)                                                     
--maxClusters n                                        maximal number of clusters to create, per domain

--minimalCoverage double                               minimal coverage for a definition to be considered as large-coverage (percentage)

--output string                                        name of the file to save new layer [default:newLayer.*]
                                                       
--overlapThreshold Double [0.3]                        if overlap measure smaller than this threshold, a cluster is accepted as a new predicate                                                      

--query comma-separated list                           list of domains to cluster; if not set, all domains are clustered

--root filePath                                        folder to place files in

--saturationTradeOff Double                            saturation trade off parameter: sim(i) >= w * sim(i+1) [default: 0.9]
                                                     
--selection predefined|model|saturation|definition     how to select the number of clusters per domain
                                                       
--similarity [RCNT|RCNTv2]                similarity measure

--weights Array[Double]                                semi-colon separated list of parameter sets; each set is a
                                                       comma-separated list of 5 doubles [attributes,attribute
                                                       distribution,connections,vertex neighbourhood, edge distribution]

--clusterHyperedges                                    should hyperedges be clusters as well (between the specified domains)
                                                     
--extractDefs                                          extract the definitions of new predicates
                                                       
-k n                                                   desired number of clusters in 'predefined' selection method is used
                                                                                   
```

### Input files

Knowledge base/graph containing the facts in a domain (*.db)
 
```
Movie(Aoceanstwelve,Anelsonpeltz)
Movie(Aplayerthe,Awhoopigoldberg)
Movie(Apelicanbriefthe,Ajuliaroberts)
Movie(Aoceanstwelve,Ajuliaroberts)
...
Gender_male(Adavidsontag)
Gender_male(Arobertculp)
Gender_female(Acynthiastevenson)
Gender_male(Afredward)
Gender_female(Adinamerrill)
...
Genre(Asoderberghsteven,Acrime)
Genre(Apakulaalanj,Adrama)
Genre(Apakulaalanj,Amystery)
Genre(Aaltmanroberti,Adrama)
Workedunder(Aminianden,Asoderberghsteven)
Workedunder(Acaseyaffleck,Asoderberghsteven)
Workedunder(Aelliottgould,Asoderberghsteven)
Workedunder(Adenzelwashington,Apakulaalanj)
...
```

Definitions file specifying the domains of objects (*.def)

```
Gender_male(person)
Gender_female(person)
Genre(person,genre)
Movie(movie,person)
Workedunder(person,person)
```

Declarations file specifying the meaning of the arguments of predicates (*.dcl)

```
Gender_male(name)
Gender_female(name)
Genre(name,attr)
Movie(name,name)
Workedunder(name,name)
```

The arguments can have the following roles:

1. `name` - identifier of an objects/instance/example; this is essentially treated as the name of an instance
2. `attr` - identifies a discrete attribute value. Attribute name is given by the name of a predicate. It needs to have exactly one `name` argument.
3. `number` - identifies a continuous attribute value. Attribute name is given by the name of a predicate. It needs to have exactly one `name` argument.

These roles influence the way a neighbourhood tree is constructed.


## Contributing

1. Fork it!
2. Create your feature branch: `git checkout -b my-new-feature`
3. Commit your changes: `git commit -am 'Add some feature'`
4. Push to the branch: `git push origin my-new-feature`
5. Submit a pull request 

## Contact

If you have any question, feel free to send then at `sebastijan.dumancic@cs.kuleuven.be`

## Citing

Please cite the following paper if you are using the code

```
@inproceedings{ijcai2017-dumancic,
  author    = {Sebastijan Dumancic, Hendrik Blockeel},
  title     = {Clustering-Based Relational Unsupervised Representation Learning with an Explicit Distributed Representation},
  booktitle = {Proceedings of the Twenty-Sixth International Joint Conference on
               Artificial Intelligence, {IJCAI-17}},
  pages     = {1631--1637},
  year      = {2017},
  doi       = {10.24963/ijcai.2017/226},
  url       = {https://doi.org/10.24963/ijcai.2017/226}
}
```

## License

Release under  Apache License, version 2.