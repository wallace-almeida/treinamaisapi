package com.treinamaisapi.service.simulado;


import com.treinamaisapi.common.dto.simulado.filtro.CapituloFiltroDTO;
import com.treinamaisapi.common.dto.simulado.filtro.PacoteFiltroSimuladoDTO;
import com.treinamaisapi.common.dto.simulado.filtro.SubcapituloFiltroDTO;
import com.treinamaisapi.common.dto.simulado.filtro.TemaFiltroDTO;
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
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.questoes_respondida.QuestaoSimulado;
import com.treinamaisapi.entity.simulado.Simulado;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.*;
import com.treinamaisapi.service.compra.pacote.PacoteCompradoService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class SimuladoService {

    private final SimuladoRepository simuladoRepository;
    private final QuestaoRepository questaoRepository;
    private final QuestaoSimuladoRepository questaoSimuladoRepository;
    private final UsuarioRepository usuarioRepository;
    private final HistoricoEstudoRepository historicoEstudoRepository;
    private final PacoteCompradoService pacoteCompradoService;
    private final PacoteCompradoRepository pacoteCompradoRepository;


    @Transactional
    public SimuladoResponse criarSimulado(CriarSimuladoRequest request, Long usuarioId) {
        Usuario usuario = usuarioRepository.findById(usuarioId)
                .orElseThrow(() -> new RuntimeException("Usuário não encontrado."));

        // 🔹 Verifica acesso
        boolean possuiAcesso = pacoteCompradoService.listarComprasAtivas(usuarioId)
                .stream()
                .anyMatch(c -> c.getConcursoId().equals(request.getConcursoId()));
        if (!possuiAcesso) {
            throw new IllegalStateException("Usuário não possui acesso a este concurso.");
        }

        // 🔹 Define nível de dificuldade
        NivelDificuldade nivel = null;
        if (request.getNivelDificuldade() != null && !request.getNivelDificuldade().isBlank()) {
            nivel = NivelDificuldade.valueOf(request.getNivelDificuldade());
        }

        int quantidadeTotal = request.getQuantidadeQuestoes() == null ? 10 : request.getQuantidadeQuestoes();

        List<Long> ids = questaoRepository.buscarIdsRandomizados(
                request.getTemaIds(),
                request.getCapituloIds(),
                request.getSubcapituloIds(),
                request.getNivelDificuldade(),
                request.getBanca(),
                quantidadeTotal
        );

        if (ids.isEmpty()) {
            throw new RuntimeException("Não foram encontradas questões com os filtros especificados.");
        }

        List<Questao> questoesSelecionadas =
                questaoRepository.findAllById(ids);

        // 🔹 Cria simulado
        Simulado simulado = Simulado.builder()
                .usuario(usuario)
                .quantidadeQuestoes(questoesSelecionadas.size())
                .tempoDuracao(request.getTempoDuracao())
                .dataCriacao(LocalDateTime.now())
                .status(StatusSimulado.EM_ANDAMENTO)
                .nivelDificuldade(request.getNivelDificuldade())
                .banca(request.getBanca())
                .temaId(request.getTemaIds().get(0))
                .capituloId((request.getCapituloIds() != null && !request.getCapituloIds().isEmpty()) ? request.getCapituloIds().get(0) : null)
                .subcapituloId((request.getSubcapituloIds() != null && !request.getSubcapituloIds().isEmpty()) ? request.getSubcapituloIds().get(0) : null)
                .build();

        simuladoRepository.save(simulado);

        // 🔹 Vincula questões ao simulado
        List<QuestaoSimulado> vinculadas = questoesSelecionadas.stream()
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
        Simulado simulado = simuladoRepository.findFirstByUsuarioIdAndStatus(usuarioId, StatusSimulado.EM_ANDAMENTO).orElseThrow(() -> new RuntimeException("Nenhum simulado em andamento encontrado"));

        List<QuestaoSimulado> questoes = questaoSimuladoRepository.findBySimuladoId(simulado.getId());

        return SimuladoExecucaoResponse.fromEntity(simulado, questoes);
    }


    @Transactional(readOnly = true)
    public List<SimuladoResponse> listarSimuladosPorUsuario(Long usuarioId) {
        List<Simulado> sims = simuladoRepository.findByUsuarioIdOrderByDataCriacaoDesc(usuarioId);
        return sims.stream().map(s -> SimuladoResponse.fromEntity(s, questaoSimuladoRepository.findBySimuladoId(s.getId()))).collect(Collectors.toList());
    }

    @Transactional
    public ResultadoSimuladoResponse responderSimulado(Long simuladoId, RespostaSimuladoRequest request) {
        Simulado simulado = simuladoRepository.findById(simuladoId).orElseThrow(() -> new RuntimeException("Simulado não encontrado"));

        if (!simulado.getStatus().equals(StatusSimulado.EM_ANDAMENTO)) {
            throw new RuntimeException("Simulado já finalizado ou não está em andamento");
        }

        double totalPontuacao = 0.0;
        int acertos = 0;

        for (RespostaQuestaoSimulado r : request.getRespostas()) {
            QuestaoSimulado qs = questaoSimuladoRepository.findBySimuladoIdAndQuestaoId(simuladoId, r.getQuestaoId()).orElseThrow(() -> new RuntimeException("Questão não encontrada no simulado"));

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
        HistoricoEstudo historico = HistoricoEstudo.builder().tipoAtividade(TipoAtividade.SIMULADO).pontuacaoObtida(pontuacaoFinal).acertos(acertos).usuario(simulado.getUsuario()).simulado(simulado).build();

        historicoEstudoRepository.save(historico);

        return visualizarResultado(simuladoId);
    }

    @Transactional(readOnly = true)
    public ResultadoSimuladoResponse visualizarResultado(Long simuladoId) {
        Simulado simulado = simuladoRepository.findById(simuladoId).orElseThrow(() -> new RuntimeException("Simulado não encontrado"));

        List<QuestaoSimulado> questoes = questaoSimuladoRepository.findBySimuladoId(simuladoId);

        int total = questoes.size();
        int acertos = (int) questoes.stream().filter(q -> Boolean.TRUE.equals(q.getCorreta())).count();

        List<FeedbackQuestaoResponse> feedbacks = questoes.stream().map(q -> {
            var questao = q.getQuestao();

            FeedbackQuestaoResponse.FeedbackQuestaoResponseBuilder fb = FeedbackQuestaoResponse.builder().questaoId(questao.getId()).enunciado(questao.getEnunciado()).respostaCorreta(questao.getRespostaCorreta()).respostaUsuario(q.getRespostaUsuario()).correta(q.getCorreta());

            // Adiciona explicação apenas se a resposta estiver errada
            if (Boolean.FALSE.equals(q.getCorreta())) {
                fb.explicacao(questao.getExplicacao());
            }

            return fb.build();
        }).collect(Collectors.toList());

        return ResultadoSimuladoResponse.builder().simuladoId(simulado.getId()).pontuacaoFinal(simulado.getPontuacaoFinal()).totalQuestoes(total).totalAcertos(acertos).totalErros(total - acertos).feedbackQuestoes(feedbacks).build();
    }


    @Transactional(readOnly = true)
    public List<PacoteFiltroSimuladoDTO> listarFiltrosPorUsuario(Long usuarioId) {

        List<PacoteComprado> pacotesAtivos = pacoteCompradoRepository.findByUsuarioIdAndAtivoTrue(usuarioId);

        return pacotesAtivos.stream().map(pc -> {
            var pacote = pc.getPacote();

            // 🔹 Monta temas, capítulos e subcapítulos
            List<TemaFiltroDTO> temas = pacote.getTemas().stream().map(tema -> TemaFiltroDTO.builder().id(tema.getId()).nome(tema.getNome()).capitulos(tema.getCapitulos().stream().map(cap -> CapituloFiltroDTO.builder().id(cap.getId()).nome(cap.getNome()).subcapitulos(cap.getSubcapitulos().stream().map(sub -> new SubcapituloFiltroDTO(sub.getId(), sub.getNome())).toList()).build()).toList()).build()).toList();

            // 🔹 Coleta bancas e níveis disponíveis (a partir das questões)
            List<String> bancas = pacote.getTemas().stream().flatMap(t -> t.getCapitulos().stream()).flatMap(c -> c.getSubcapitulos().stream()).flatMap(s -> s.getQuestoes().stream()).map(Questao::getBanca).filter(Objects::nonNull).distinct().toList();

            List<String> niveis = pacote.getTemas().stream().flatMap(t -> t.getCapitulos().stream()).flatMap(c -> c.getSubcapitulos().stream()).flatMap(s -> s.getQuestoes().stream()).map(q -> q.getNivelDificuldade().name()).distinct().toList();

            return PacoteFiltroSimuladoDTO.builder().pacoteId(pacote.getId()).nomePacote(pacote.getNome()).concursoId(pacote.getConcurso().getId()).nomeConcurso(pacote.getConcurso().getNome()).temas(temas).bancasDisponiveis(bancas).niveisDisponiveis(niveis).build();
        }).toList();
    }

}

