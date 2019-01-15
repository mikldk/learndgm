#ifndef CLASS_CHERRYMODEL_H
#define CLASS_CHERRYMODEL_H

#include <vector>

class CherryModel {
  
protected:
  std::vector< std::vector<int> > m_cliques;
  std::vector< std::vector<int> > m_seps;
  std::vector<int> m_parents;
  
public:
  CherryModel(const std::vector< std::vector<int> >& cliques,
              const std::vector< std::vector<int> >& seps,
              const std::vector<int>& parents) {
    
    m_cliques = cliques;
    m_seps = seps;
    m_parents = parents;
  }
  
  const std::vector< std::vector<int> > get_cliques() {
    return m_cliques;
  }
  
  const std::vector< std::vector<int> > get_seps() {
    return m_seps;
  }
  
  const std::vector<int> get_parents() {
    return m_parents; 
  }
};


class CherryModelUnused : public CherryModel  {
  
protected:
  std::vector<int> m_unused;
  
public:
  CherryModelUnused(const std::vector< std::vector<int> >& cliques,
                                   const std::vector< std::vector<int> >& seps,
                                   const std::vector<int>& parents,
                                   const std::vector<int>& unused) :
  
  CherryModel(cliques, seps, parents) {
    m_unused = unused;
  }

  const std::vector<int> get_unused() {
    return m_unused; 
  }
};



class CherryModelMI : public CherryModel  {
  
protected:
  std::vector<double> m_cliques_mi;
  std::vector<double> m_seps_mi;
  
public:
  CherryModelMI(const std::vector< std::vector<int> >& cliques,
                const std::vector< std::vector<int> >& seps,
                const std::vector<int>& parents) :
  CherryModel(cliques, seps, parents) {
    
  }
  
  void set_cliques_mi(const std::vector<double>& cliques_mi) {
    m_cliques_mi = cliques_mi;
  }
  
  void set_seps_mi(const std::vector<double>& seps_mi) {
    m_seps_mi = seps_mi;
  }
  
  const std::vector<double> get_cliques_mi() {
    return m_cliques_mi; 
  }
  
  const std::vector<double> get_seps_mi() {
    return m_seps_mi; 
  }
};

#endif
