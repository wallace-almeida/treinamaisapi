package com.treinamaisapi.service.simulado;

import com.treinamaisapi.common.dto.questao.request.QuestaoRequest;
import com.treinamaisapi.common.dto.questao.response.QuestaoResponse;
import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.request.RespostaQuestaoSimulado;
import com.treinamaisapi.common.dto.simulado.request.RespostaSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.response.FeedbackQuestaoResponse;
import com.treinamaisapi.common.dto.simulado.response.ResultadoSimuladoResponse;
import com.treinamaisapi.common.dto.simulado.response.SimuladoResponse;
import com.treinamaisapi.entity.enums.NivelDificuldade;
import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.questoes_respondida.QuestaoSimulado;
import com.treinamaisapi.entity.simulado.Simulado;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

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


    // 1️⃣ Criar simulado automaticamente
    public SimuladoResponse criarSimulado(CriarSimuladoRequest request, Long usuarioId) {
        Usuario usuario = usuarioRepository.findById(usuarioId)
                .orElseThrow(() -> new RuntimeException("Usuário não encontrado"));

        NivelDificuldade nivel = null;
        if (request.getNivelDificuldade() != null) {
            nivel = NivelDificuldade.valueOf(request.getNivelDificuldade());
        }

        List<Questao> questoes = questaoRepository.buscarPorFiltros(
                request.getTemaId(),
                request.getCapituloId(),
                request.getSubcapituloId(),
                nivel,
                request.getBanca(),
                request.getQuantidadeQuestoes()
        );

        // Verifica se encontrou questões suficientes
        if (questoes.isEmpty()) {
            throw new RuntimeException("Não foram encontradas questões com os filtros especificados");
        }

        Simulado simulado = Simulado.builder()
                .usuario(usuario)
                .quantidadeQuestoes(questoes.size())
                .tempoDuracao(request.getTempoDuracao())
                .dataCriacao(LocalDateTime.now())
                .temaId(request.getTemaId())
                .capituloId(request.getCapituloId())
                .subcapituloId(request.getSubcapituloId())
                .nivelDificuldade(request.getNivelDificuldade())
                .banca(request.getBanca())
                .build();

        simuladoRepository.save(simulado);

        // CORREÇÃO: Define valores padrão para os campos que não podem ser nulos
        List<QuestaoSimulado> questaoSimulados = questoes.stream()
                .map(q -> QuestaoSimulado.builder()
                        .simulado(simulado)
                        .questao(q)
                        .correta(false) // Define como false por padrão
                        .respostaUsuario(null) // Explicitamente null
                        .pontuacaoObtida(0.0) // Define 0 como padrão
                        .build())
                .collect(Collectors.toList());

        questaoSimuladoRepository.saveAll(questaoSimulados);

        return SimuladoResponse.fromEntity(simulado, questaoSimulados);
    }

    // 2️⃣ Listar simulados de um usuário
    public List<SimuladoResponse> listarSimuladosPorUsuario(Long usuarioId) {
        return simuladoRepository.findByUsuarioId(usuarioId)
                .stream()
                .map(s -> SimuladoResponse.fromEntity(s, s.getQuestoes()))
                .collect(Collectors.toList());
    }

    // 3️⃣ Enviar respostas e calcular resultado
    public ResultadoSimuladoResponse responderSimulado(Long simuladoId, RespostaSimuladoRequest request) {
        Simulado simulado = simuladoRepository.findById(simuladoId)
                .orElseThrow(() -> new RuntimeException("Simulado não encontrado"));

        double pontuacao = 0;
        int acertos = 0;

        for (RespostaQuestaoSimulado r : request.getRespostas()) {
            QuestaoSimulado qs = questaoSimuladoRepository
                    .findBySimuladoIdAndQuestaoId(simuladoId, r.getQuestaoId())
                    .orElseThrow(() -> new RuntimeException("Questão não encontrada no simulado"));

            boolean correta = r.getRespostaUsuario().equalsIgnoreCase(qs.getQuestao().getRespostaCorreta());
            qs.setRespostaUsuario(r.getRespostaUsuario());
            qs.setCorreta(correta);
            qs.setPontuacaoObtida(correta ? 1.0 : 0.0);
            questaoSimuladoRepository.save(qs);

            if (correta) acertos++;
            pontuacao += qs.getPontuacaoObtida();
        }

        double pontuacaoFinal = (pontuacao / request.getRespostas().size()) * 100;
        simulado.setPontuacaoFinal(pontuacaoFinal);
        simuladoRepository.save(simulado);

        return visualizarResultado(simuladoId);
    }

    // 4️⃣ Retornar resultado completo com feedback
    public ResultadoSimuladoResponse visualizarResultado(Long simuladoId) {
        Simulado simulado = simuladoRepository.findById(simuladoId)
                .orElseThrow(() -> new RuntimeException("Simulado não encontrado"));

        List<QuestaoSimulado> questoes = questaoSimuladoRepository.findBySimuladoId(simuladoId);

        int total = questoes.size();
        int acertos = (int) questoes.stream().filter(QuestaoSimulado::getCorreta).count();

        List<FeedbackQuestaoResponse> feedbacks = questoes.stream()
                .map(q -> FeedbackQuestaoResponse.builder()
                        .questaoId(q.getQuestao().getId())
                        .enunciado(q.getQuestao().getEnunciado())
                        .respostaCorreta(q.getQuestao().getRespostaCorreta())
                        .respostaUsuario(q.getRespostaUsuario())
                        .correta(q.getCorreta())
                        .build())
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

