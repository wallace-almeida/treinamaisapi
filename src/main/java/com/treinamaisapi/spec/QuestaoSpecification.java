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
        return (root, query, cb) -> {

            List<Predicate> preds = new ArrayList<>();

            var sc = root.join("subcapitulo");
            var c = sc.join("capitulo");
            var t = c.join("tema");

            if (!isEmpty(filtro.getTemaIds())) {
                preds.add(t.get("id").in(filtro.getTemaIds()));
            }

            if (!isEmpty(filtro.getCapituloIds())) {
                preds.add(c.get("id").in(filtro.getCapituloIds()));
            }

            if (!isEmpty(filtro.getSubcapituloIds())) {
                preds.add(sc.get("id").in(filtro.getSubcapituloIds()));
            }

            if (!isEmpty(filtro.getBancas())) {
                preds.add(root.get("banca").in(filtro.getBancas()));
            }

            if (!isEmpty(filtro.getNiveis())) {
                preds.add(root.get("nivelDificuldade").in(
                        filtro.getNiveis().stream()
                                .map(NivelDificuldade::valueOf)
                                .toList()
                ));
            }

            return cb.and(preds.toArray(new Predicate[0]));
        };
    }

    private static boolean isEmpty(List<?> list) {
        return list == null || list.isEmpty();
    }
}
