package com.treinamaisapi.service.questao;

import com.treinamaisapi.common.dto.questao.request.QuestaoRequest;
import com.treinamaisapi.common.dto.questao.response.QuestaoResponse;
import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.repository.QuestaoRepository;
import com.treinamaisapi.repository.SubCapituloRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
public class QuestaoService {

    private final QuestaoRepository questaoRepository;
    private final SubCapituloRepository subcapituloRepository;

    public List<QuestaoResponse> criarLote(List<QuestaoRequest> requests) {
        List<Questao> questoes = new ArrayList<>();

        for (QuestaoRequest req : requests) {
            var subcapitulo = subcapituloRepository.findById(req.getSubcapituloId())
                    .orElseThrow(() -> new IllegalArgumentException("Subcapítulo não encontrado"));

            var questao = Questao.builder()
                    .enunciado(req.getEnunciado())
                    .alternativaA(req.getAlternativaA())
                    .alternativaB(req.getAlternativaB())
                    .alternativaC(req.getAlternativaC())
                    .alternativaD(req.getAlternativaD())
                    .respostaCorreta(req.getRespostaCorreta())
                    .nivelDificuldade(req.getNivelDificuldade())
                    .banca(req.getBanca())
                    .subcapitulo(subcapitulo)
                    .build();

            questoes.add(questao);
        }

        // persiste tudo de uma vez para obter os IDs gerados
        List<Questao> saved = questaoRepository.saveAll(questoes);

        // mapear para QuestaoResponse com todas as propriedades (incluindo alternativas)
        return saved.stream().map(q -> new QuestaoResponse(
                q.getId(),
                q.getEnunciado(),
                q.getAlternativaA(),
                q.getAlternativaB(),
                q.getAlternativaC(),
                q.getAlternativaD(),
                q.getRespostaCorreta(),
                q.getBanca(),
                q.getNivelDificuldade() != null ? q.getNivelDificuldade().name() : null,
                q.getSubcapitulo() != null ? q.getSubcapitulo().getNome() : null,
                q.getSubcapitulo() != null && q.getSubcapitulo().getCapitulo() != null
                        ? q.getSubcapitulo().getCapitulo().getNome() : null,
                q.getSubcapitulo() != null && q.getSubcapitulo().getCapitulo() != null
                        && q.getSubcapitulo().getCapitulo().getTema() != null
                        ? q.getSubcapitulo().getCapitulo().getTema().getNome() : null
        )).toList();
    }


    public List<QuestaoResponse> listarPorFiltro(Long temaId, Long capituloId, Long subcapituloId,
                                                 String banca, String nivel) {
        return questaoRepository.findAll().stream()
                .filter(q -> temaId == null || q.getSubcapitulo().getCapitulo().getTema().getId().equals(temaId))
                .filter(q -> capituloId == null || q.getSubcapitulo().getCapitulo().getId().equals(capituloId))
                .filter(q -> subcapituloId == null || q.getSubcapitulo().getId().equals(subcapituloId))
                .filter(q -> banca == null || q.getBanca().equalsIgnoreCase(banca))
                .filter(q -> nivel == null || q.getNivelDificuldade().name().equalsIgnoreCase(nivel))
                .map(q -> new QuestaoResponse(
                        q.getId(),
                        q.getEnunciado(),
                        q.getAlternativaA(),
                        q.getAlternativaB(),
                        q.getAlternativaC(),
                        q.getAlternativaD(),
                        q.getRespostaCorreta(),
                        q.getBanca(),
                        q.getNivelDificuldade().name(),
                        q.getSubcapitulo().getNome(),
                        q.getSubcapitulo().getCapitulo().getNome(),
                        q.getSubcapitulo().getCapitulo().getTema().getNome()
                )).toList();
    }

}

