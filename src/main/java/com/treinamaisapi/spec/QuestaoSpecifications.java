package com.treinamaisapi.spec;



import com.treinamaisapi.entity.questoes.Questao;

import com.treinamaisapi.filter.questao.QuestaoFiltroDTO;
import org.springframework.data.jpa.domain.Specification;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

public class QuestaoSpecification {

    public static Specification<Questao> filtrar(QuestaoFiltroDTO filtro) {
        return (root, query, builder) -> {
            List<Predicate> predicates = new ArrayList<>();

            if (filtro.id() != null) {
                predicates.add(builder.equal(root.get("id"), filtro.id()));
            }

            if (filtro.enunciado() != null && !filtro.enunciado().isBlank()) {
                predicates.add(builder.like(
                        builder.lower(root.get("enunciado")),
                        "%" + filtro.enunciado().toLowerCase() + "%"
                ));
            }

            if (filtro.banca() != null) {
                predicates.add(builder.equal(root.get("banca"), filtro.banca()));
            }

            if (filtro.nivelDificuldade() != null) {
                predicates.add(builder.equal(root.get("nivelDificuldade"), filtro.nivelDificuldade()));
            }

            if (filtro.subcapituloId() != null) {
                predicates.add(builder.equal(root.get("subcapitulo").get("id"), filtro.subcapituloId()));
            }

            return builder.and(predicates.toArray(new Predicate[0]));
        };
    }
}

