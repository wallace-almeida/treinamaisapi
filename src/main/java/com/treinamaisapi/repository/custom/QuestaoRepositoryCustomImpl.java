package com.treinamaisapi.repository.custom;

import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;
import com.treinamaisapi.entity.enums.NivelDificuldade;
import com.treinamaisapi.entity.questoes.Questao;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
@RequiredArgsConstructor
public class QuestaoRepositoryCustomImpl implements QuestaoRepositoryCustom {

    private final EntityManager em;

    @Override
    public List<Long> findIdsByFiltro(CriarSimuladoRequest filtro) {
        CriteriaBuilder cb = em.getCriteriaBuilder();
        CriteriaQuery<Long> cq = cb.createQuery(Long.class);

        Root<Questao> root = cq.from(Questao.class);
        Join<Object, Object> sc = root.join("subcapitulo");
        Join<Object, Object> c  = sc.join("capitulo");
        Join<Object, Object> t  = c.join("tema");

        List<Predicate> preds = new ArrayList<>();

        if (filtro.getTemaIds() != null && !filtro.getTemaIds().isEmpty()) {
            preds.add(t.get("id").in(filtro.getTemaIds()));
        }
        if (filtro.getCapituloIds() != null && !filtro.getCapituloIds().isEmpty()) {
            preds.add(c.get("id").in(filtro.getCapituloIds()));
        }
        if (filtro.getSubcapituloIds() != null && !filtro.getSubcapituloIds().isEmpty()) {
            preds.add(sc.get("id").in(filtro.getSubcapituloIds()));
        }
        if (filtro.getBancas() != null && !filtro.getBancas().isEmpty()) {
            preds.add(root.get("banca").in(filtro.getBancas()));
        }
        if (filtro.getNiveis() != null && !filtro.getNiveis().isEmpty()) {
            List<NivelDificuldade> niveis = filtro.getNiveis().stream()
                    .map(String::trim)
                    .filter(s -> !s.isBlank())
                    .map(String::toUpperCase)
                    .map(NivelDificuldade::valueOf)
                    .toList();
            preds.add(root.get("nivelDificuldade").in(niveis));
        }

        cq.select(root.get("id")).where(cb.and(preds.toArray(new Predicate[0])));

        // evita duplicar por join
        cq.distinct(true);

        return em.createQuery(cq).getResultList();
    }
}

