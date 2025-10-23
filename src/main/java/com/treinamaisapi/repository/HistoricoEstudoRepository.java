package com.treinamaisapi.repository;


import com.treinamaisapi.entity.historico_estudo.HistoricoEstudo;
import com.treinamaisapi.entity.questoes_respondida.QuestaoSimulado;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface HistoricoEstudoRepository extends JpaRepository<HistoricoEstudo, Long>, JpaSpecificationExecutor<HistoricoEstudo>,  PagingAndSortingRepository<HistoricoEstudo, Long>{

    List<HistoricoEstudo> findByUsuarioIdOrderByDataEstudoDesc(Long usuarioId);


}
