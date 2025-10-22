package com.treinamaisapi.repository;


import com.treinamaisapi.entity.questoes_respondida.QuestaoSimulado;
import com.treinamaisapi.entity.simulado.Simulado;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface QuestaoSimuladoRepository extends JpaRepository<QuestaoSimulado, Long>, JpaSpecificationExecutor<QuestaoSimulado>,  PagingAndSortingRepository<QuestaoSimulado, Long>{

    List<QuestaoSimulado> findBySimuladoId(Long simuladoId);

    @Query("SELECT qs FROM QuestaoSimulado qs WHERE qs.simulado.id = :simuladoId AND qs.questao.id = :questaoId")
    Optional<QuestaoSimulado> findBySimuladoIdAndQuestaoId(
            @Param("simuladoId") Long simuladoId,
            @Param("questaoId") Long questaoId
    );


}
