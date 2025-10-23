package com.treinamaisapi.service.simulado;

import com.treinamaisapi.common.dto.questao.request.QuestaoRequest;
import com.treinamaisapi.common.dto.questao.response.QuestaoResponse;
import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.request.RespostaQuestaoSimulado;
import com.treinamaisapi.common.dto.simulado.request.RespostaSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.response.FeedbackQuestaoResponse;
import com.treinamaisapi.common.dto.simulado.response.ResultadoSimuladoResponse;
import com.treinamaisapi.common.dto.simulado.response.SimuladoExecucaoResponse;
import com.treinamaisapi.common.dto.simulado.response.SimuladoResponse;
import com.treinamaisapi.entity.enums.NivelDificuldade;
import com.treinamaisapi.entity.enums.StatusSimulado;
import com.treinamaisapi.entity.enums.TipoAtividade;
import com.treinamaisapi.entity.historico_estudo.HistoricoEstudo;
import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.questoes_respondida.QuestaoSimulado;
import com.treinamaisapi.entity.simulado.Simulado;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class SimuladoService {

    private final SimuladoRepository simuladoRepository;
    private final QuestaoRepository questaoRepository;
    private final QuestaoSimuladoRepository questaoSimuladoRepository;
    private final UsuarioRepository usuarioRepository;
    private final HistoricoEstudoRepository historicoEstudoRepository;

    @Transactional
    public SimuladoResponse criarSimulado(CriarSimuladoRequest request, Long usuarioId) {
        Usuario usuario = usuarioRepository.findById(usuarioId)
                .orElseThrow(() -> new RuntimeException("Usuário não encontrado"));

        NivelDificuldade nivel = null;
        if (request.getNivelDificuldade() != null && !request.getNivelDificuldade().isBlank()) {
            nivel = NivelDificuldade.valueOf(request.getNivelDificuldade());
        }

        int quantidade = request.getQuantidadeQuestoes() == null ? 10 : request.getQuantidadeQuestoes();

        List<Questao> questoes = questaoRepository.buscarPorFiltros(
                request.getTemaId(),
                request.getCapituloId(),
                request.getSubcapituloId(),
                nivel,
                request.getBanca(),
                quantidade
        );

        if (questoes.isEmpty()) {
            throw new RuntimeException("Não foram encontradas questões com os filtros especificados");
        }

        Simulado simulado = Simulado.builder()
                .usuario(usuario)
                .quantidadeQuestoes(questoes.size())
                .tempoDuracao(request.getTempoDuracao())
                .dataCriacao(LocalDateTime.now())
                .status(StatusSimulado.EM_ANDAMENTO)
                .temaId(request.getTemaId())
                .capituloId(request.getCapituloId())
                .subcapituloId(request.getSubcapituloId())
                .nivelDificuldade(request.getNivelDificuldade())
                .banca(request.getBanca())
                .build();

        simuladoRepository.save(simulado);

        List<QuestaoSimulado> vinculadas = questoes.stream()
                .map(q -> QuestaoSimulado.builder()
                        .simulado(simulado)
                        .questao(q)
                        .respostaUsuario(null)
                        .correta(null)
                        .pontuacaoObtida(0.0)
                        .build())
                .collect(Collectors.toList());

        questaoSimuladoRepository.saveAll(vinculadas);

        return SimuladoResponse.fromEntity(simulado, vinculadas);
    }

    @Transactional(readOnly = true)
    public SimuladoExecucaoResponse buscarSimuladoAtivo(Long usuarioId) {
        Simulado simulado = simuladoRepository.findFirstByUsuarioIdAndStatus(usuarioId, StatusSimulado.EM_ANDAMENTO)
                .orElseThrow(() -> new RuntimeException("Nenhum simulado em andamento encontrado"));

        List<QuestaoSimulado> questoes = questaoSimuladoRepository.findBySimuladoId(simulado.getId());

        return SimuladoExecucaoResponse.fromEntity(simulado, questoes);
    }


    @Transactional(readOnly = true)
    public List<SimuladoResponse> listarSimuladosPorUsuario(Long usuarioId) {
        List<Simulado> sims = simuladoRepository.findByUsuarioIdOrderByDataCriacaoDesc(usuarioId);
        return sims.stream()
                .map(s -> SimuladoResponse.fromEntity(s, questaoSimuladoRepository.findBySimuladoId(s.getId())))
                .collect(Collectors.toList());
    }

    @Transactional
    public ResultadoSimuladoResponse responderSimulado(Long simuladoId, RespostaSimuladoRequest request) {
        Simulado simulado = simuladoRepository.findById(simuladoId)
                .orElseThrow(() -> new RuntimeException("Simulado não encontrado"));

        if (!simulado.getStatus().equals(StatusSimulado.EM_ANDAMENTO)) {
            throw new RuntimeException("Simulado já finalizado ou não está em andamento");
        }

        double totalPontuacao = 0.0;
        int acertos = 0;

        for (RespostaQuestaoSimulado r : request.getRespostas()) {
            QuestaoSimulado qs = questaoSimuladoRepository
                    .findBySimuladoIdAndQuestaoId(simuladoId, r.getQuestaoId())
                    .orElseThrow(() -> new RuntimeException("Questão não encontrada no simulado"));

            boolean correta = qs.getQuestao().getRespostaCorreta().equalsIgnoreCase(r.getRespostaUsuario());
            qs.setRespostaUsuario(r.getRespostaUsuario());
            qs.setCorreta(correta);
            qs.setPontuacaoObtida(correta ? 1.0 : 0.0);
            questaoSimuladoRepository.save(qs);

            if (correta) acertos++;
            totalPontuacao += qs.getPontuacaoObtida();
        }

        double pontuacaoFinal = (totalPontuacao / request.getRespostas().size()) * 100.0;
        simulado.setPontuacaoFinal(pontuacaoFinal);
        simulado.setStatus(StatusSimulado.FINALIZADO);
        simuladoRepository.save(simulado);

        // criar entrada no histórico
        HistoricoEstudo historico = HistoricoEstudo.builder()
                .tipoAtividade(TipoAtividade.SIMULADO)
                .pontuacaoObtida(pontuacaoFinal)
                .acertos(acertos)
                .usuario(simulado.getUsuario())
                .simulado(simulado)
                .build();

        historicoEstudoRepository.save(historico);

        return visualizarResultado(simuladoId);
    }

    @Transactional(readOnly = true)
    public ResultadoSimuladoResponse visualizarResultado(Long simuladoId) {
        Simulado simulado = simuladoRepository.findById(simuladoId)
                .orElseThrow(() -> new RuntimeException("Simulado não encontrado"));

        List<QuestaoSimulado> questoes = questaoSimuladoRepository.findBySimuladoId(simuladoId);

        int total = questoes.size();
        int acertos = (int) questoes.stream().filter(q -> Boolean.TRUE.equals(q.getCorreta())).count();

        List<FeedbackQuestaoResponse> feedbacks = questoes.stream()
                .map(q -> {
                    var questao = q.getQuestao();

                    FeedbackQuestaoResponse.FeedbackQuestaoResponseBuilder fb = FeedbackQuestaoResponse.builder()
                            .questaoId(questao.getId())
                            .enunciado(questao.getEnunciado())
                            .respostaCorreta(questao.getRespostaCorreta())
                            .respostaUsuario(q.getRespostaUsuario())
                            .correta(q.getCorreta());

                    // Adiciona explicação apenas se a resposta estiver errada
                    if (Boolean.FALSE.equals(q.getCorreta())) {
                        fb.explicacao(questao.getExplicacao());
                    }

                    return fb.build();
                })
                .collect(Collectors.toList());

        return ResultadoSimuladoResponse.builder()
                .simuladoId(simulado.getId())
                .pontuacaoFinal(simulado.getPontuacaoFinal())
                .totalQuestoes(total)
                .totalAcertos(acertos)
                .totalErros(total - acertos)
                .feedbackQuestoes(feedbacks)
                .build();
    }
}

