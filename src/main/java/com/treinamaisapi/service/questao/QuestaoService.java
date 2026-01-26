package com.treinamaisapi.service.questao;

import com.treinamaisapi.common.dto.questao.request.QuestaoRequest;
import com.treinamaisapi.common.dto.questao.request.QuestaoUpdateRequest;
import com.treinamaisapi.common.dto.questao.response.QuestaoResponse;
import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.common.exception.NotFoundException;
import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.repository.QuestaoRepository;
import com.treinamaisapi.repository.SubCapituloRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
                    .orElseThrow(() -> new NotFoundException("Subcapítulo não encontrado"));

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
                    .explicacao(req.getExplicacao())
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

    public void deletar(Long id) {
        if (!questaoRepository.existsById(id)) {
            throw new NotFoundException("Questão não encontrada");
        }
        try {
            questaoRepository.deleteById(id);
        } catch (DataIntegrityViolationException e) {
            throw new BusinessException("Não é possível excluir: existem cartões vinculados a esta questão.");
        }
    }



    @Transactional
    public QuestaoResponse atualizarParcial(Long id, QuestaoUpdateRequest req) {
        Questao questao = questaoRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Questão não encontrada"));

        if (req.getEnunciado() != null) questao.setEnunciado(req.getEnunciado());
        if (req.getAlternativaA() != null) questao.setAlternativaA(req.getAlternativaA());
        if (req.getAlternativaB() != null) questao.setAlternativaB(req.getAlternativaB());
        if (req.getAlternativaC() != null) questao.setAlternativaC(req.getAlternativaC());
        if (req.getAlternativaD() != null) questao.setAlternativaD(req.getAlternativaD());
        if (req.getRespostaCorreta() != null) questao.setRespostaCorreta(req.getRespostaCorreta());
        if (req.getExplicacao() != null) questao.setExplicacao(req.getExplicacao());
        if (req.getNivelDificuldade() != null) questao.setNivelDificuldade(req.getNivelDificuldade());
        if (req.getBanca() != null) questao.setBanca(req.getBanca());

        if (req.getSubcapituloId() != null) {
            var subcapitulo = subcapituloRepository.findById(req.getSubcapituloId())
                    .orElseThrow(() -> new NotFoundException("Subcapítulo não encontrado"));
            questao.setSubcapitulo(subcapitulo);
        }

        // como estamos em transação, nem precisa chamar save() obrigatoriamente,
        // mas pode manter por clareza:
        Questao saved = questaoRepository.save(questao);

        return toResponse(saved);
    }

    private QuestaoResponse toResponse(Questao q) {
        return new QuestaoResponse(
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
        );
    }



}

