package com.treinamaisapi.spec;



import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;
import com.treinamaisapi.entity.enums.NivelDificuldade;
import com.treinamaisapi.entity.questoes.Questao;

import org.springframework.data.jpa.domain.Specification;

import jakarta.persistence.criteria.Predicate;
import java.util.ArrayList;
import java.util.List;

public class QuestaoSpecification {

    public static Specification<Questao> filtrar(CriarSimuladoRequest filtro) {
        return (root, query, builder) -> {
            List<Predicate> predicates = new ArrayList<>();

            var sc = root.join("subcapitulo");
            var c = sc.join("capitulo");
            var t = c.join("tema");

            if (filtro.getTemaIds() != null && !filtro.getTemaIds().isEmpty()) {
                predicates.add(t.get("id").in(filtro.getTemaIds()));
            }

            if (filtro.getCapituloIds() != null && !filtro.getCapituloIds().isEmpty()) {
                predicates.add(c.get("id").in(filtro.getCapituloIds()));
            }

            if (filtro.getSubcapituloIds() != null && !filtro.getSubcapituloIds().isEmpty()) {
                predicates.add(sc.get("id").in(filtro.getSubcapituloIds()));
            }

            if (filtro.getBanca() != null && !filtro.getBanca().isBlank()) {
                predicates.add(builder.equal(root.get("banca"), filtro.getBanca()));
            }

            if (filtro.getNivelDificuldade() != null && !filtro.getNivelDificuldade().isBlank()) {
                try {
                    predicates.add(builder.equal(
                            root.get("nivelDificuldade"),
                            NivelDificuldade.valueOf(filtro.getNivelDificuldade())
                    ));
                } catch (IllegalArgumentException ex) {
                    throw new RuntimeException("Nível de dificuldade inválido: " + filtro.getNivelDificuldade());
                }
            }

            return builder.and(predicates.toArray(new Predicate[0]));
        };
    }

}