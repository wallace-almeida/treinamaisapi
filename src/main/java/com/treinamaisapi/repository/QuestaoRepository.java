package com.treinamaisapi.repository;


import com.treinamaisapi.entity.capitulo.Capitulo;
import com.treinamaisapi.entity.questoes.Questao;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface QuestaoRepository extends JpaRepository<Questao, Long>, JpaSpecificationExecutor<Questao>,  PagingAndSortingRepository<Questao, Long>{




}
