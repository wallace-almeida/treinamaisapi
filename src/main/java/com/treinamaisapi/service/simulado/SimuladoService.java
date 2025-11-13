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
import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.common.exception.NotFoundException;
import com.treinamaisapi.entity.enums.StatusSimulado;
import com.treinamaisapi.entity.enums.TipoAtividade;
import com.treinamaisapi.entity.historico_estudo.HistoricoEstudo;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import com.treinamaisapi.entity.questao_historico_usuario.QuestaoHistoricoUsuario;
import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.questoes_respondida.QuestaoSimulado;
import com.treinamaisapi.entity.simulado.Simulado;
import com.treinamaisapi.entity.usuarios.Usuario;

import com.treinamaisapi.repository.*;
import com.treinamaisapi.service.compra.pacote.PacoteCompradoService;
import com.treinamaisapi.service.simulado.auxiliar.QuestaoBalanceService;
import com.treinamaisapi.service.simulado.auxiliar.QuestaoFraquezaService;
import com.treinamaisapi.service.simulado.auxiliar.QuestaoHistoricoService;
import com.treinamaisapi.service.simulado.auxiliar.QuestaoSelectorService;
import com.treinamaisapi.spec.QuestaoSpecification;
import lombok.RequiredArgsConstructor;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
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
    private final QuestaoSelectorService questaoSelectorService;
    private final QuestaoBalanceService questaoBalanceService;
    private final QuestaoHistoricoService questaoHistoricoService;
    private final QuestaoFraquezaService questaoFraquezaService;
    private final QuestaoHistoricoUsuarioRepository questaoHistoricoUsuarioRepository;


    @Transactional
    public SimuladoExecucaoResponse criarSimulado(CriarSimuladoRequest request, Long usuarioId) {

        // 1) Carrega usuário
        Usuario usuario = usuarioRepository.findById(usuarioId)
                .orElseThrow(() -> new NotFoundException("Usuário não encontrado."));

        // 2) Valida compra
        boolean possuiAcesso = pacoteCompradoService.listarComprasAtivas(usuarioId)
                .stream().anyMatch(c -> c.getConcursoId().equals(request.getConcursoId()));

        if (!possuiAcesso) {
            throw new BusinessException("Usuário não possui acesso a este concurso.");
        }

        // 3) Define quantidade de questões
        int quantidadeTotal = request.getQuantidadeQuestoes() == null ? 10 : request.getQuantidadeQuestoes();

        // 4) Busca questões baseadas nos filtros
        List<Questao> questoesFiltradas = questaoRepository.findAll(
                QuestaoSpecification.filtrar(request)
        );

        if (questoesFiltradas.isEmpty()) {
            throw new RuntimeException("Não foram encontradas questões com os filtros especificados.");
        }

        // ✅ Remove duplicadas logo no início
        questoesFiltradas = questoesFiltradas.stream()
                .distinct()
                .toList();

        // 5) Seleção inicial
        List<Questao> questoesSelecionadas = questaoSelectorService.selecionar(
                questoesFiltradas, usuario, quantidadeTotal, request
        );

        // 6) Modo inteligente
        // 🔹 Primeiro busca questões de fraqueza
        questoesSelecionadas = questaoFraquezaService.buscarQuestoesDeFraqueza(usuario, questoesSelecionadas, quantidadeTotal);

        // 🔹 Depois filtra questões não respondidas
        questoesSelecionadas = questaoHistoricoService.filtrarNaoRespondidas(usuario, questoesSelecionadas);

        // 🔹 Por fim, balanceia questões (histórico / balanceamento geral)
        questoesSelecionadas = questaoBalanceService.balancear(questoesSelecionadas, request);

        // ------------------------------------------------------------------
        // ✅ Remove duplicadas novamente
        questoesSelecionadas = new ArrayList<>(questoesSelecionadas.stream()
                .distinct()
                .toList());

        // 🔥 Se faltarem questões, completa com restantes
        if (questoesSelecionadas.size() < quantidadeTotal) {
            List<Questao> restantes = new ArrayList<>(questoesFiltradas);
            restantes.removeAll(questoesSelecionadas);

            for (Questao q : restantes) {
                if (questoesSelecionadas.size() >= quantidadeTotal) break;
                questoesSelecionadas.add(q);
            }
        }

        // Garante máximo
        questoesSelecionadas = questoesSelecionadas.stream()
                .limit(quantidadeTotal)
                .toList();
        // ------------------------------------------------------------------

        if (questoesSelecionadas.isEmpty()) {
            throw new RuntimeException("Nenhuma questão disponível após filtragem inteligente.");
        }

        // 7) Cria simulado
        Simulado simulado = Simulado.builder()
                .usuario(usuario)
                .quantidadeQuestoes(questoesSelecionadas.size())
                .tempoDuracao(request.getTempoDuracao())
                .dataCriacao(LocalDateTime.now())
                .status(StatusSimulado.EM_ANDAMENTO)
                .bancas(request.getBancas() != null ? new ArrayList<>(request.getBancas()) : null)
                .niveis(request.getNiveis() != null ? new ArrayList<>(request.getNiveis()) : null)
                .temaIds(request.getTemaIds() != null ? new ArrayList<>(request.getTemaIds()) : null)
                .capituloIds(request.getCapituloIds() != null ? new ArrayList<>(request.getCapituloIds()) : null)
                .subcapituloIds(request.getSubcapituloIds() != null ? new ArrayList<>(request.getSubcapituloIds()) : null)
                .inteligente(true)
                .balanceado(true)
                .prioridadeFraquezas(true)
                .build();

        simuladoRepository.save(simulado);

        // 8) Criação de vínculo QuestaoSimulado
        AtomicInteger idx = new AtomicInteger(1);
        List<QuestaoSimulado> vinculadas = questoesSelecionadas.stream()
                .map(q -> QuestaoSimulado.builder()
                        .simulado(simulado)
                        .questao(q)
                        .pontuacaoObtida(0.0)
                        .respondida(false)
                        .ordem(idx.getAndIncrement())
                        .build()
                ).toList();

        questaoSimuladoRepository.saveAll(vinculadas);

        // 9) Retorna simulado pronto para execução
        return SimuladoExecucaoResponse.fromEntity(simulado, vinculadas);
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
        Simulado simulado = simuladoRepository.findById(simuladoId)
                .orElseThrow(() -> new RuntimeException("Simulado não encontrado"));

        if (!simulado.getStatus().equals(StatusSimulado.EM_ANDAMENTO)) {
            throw new NotFoundException("Simulado já finalizado");
        }

        double totalPontuacao = 0.0;
        int acertos = 0;
        LocalDateTime agora = LocalDateTime.now();

        // ✅ Lista para salvar todos os históricos de uma vez
        List<QuestaoHistoricoUsuario> historicos = new ArrayList<>();

        for (RespostaQuestaoSimulado r : request.getRespostas()) {
            QuestaoSimulado qs = questaoSimuladoRepository
                    .findBySimuladoIdAndQuestaoId(simuladoId, r.getQuestaoId())
                    .orElseThrow(() -> new NotFoundException("Questão não encontrada no simulado"));

            boolean correta = qs.getQuestao().getRespostaCorreta().equalsIgnoreCase(r.getRespostaUsuario());
            qs.setRespostaUsuario(r.getRespostaUsuario());
            qs.setCorreta(correta);
            qs.setPontuacaoObtida(correta ? 1.0 : 0.0);
            qs.setRespondida(true);
            questaoSimuladoRepository.save(qs);

            // 🔥 Monta o histórico, mas não salva ainda
            QuestaoHistoricoUsuario historicoQuestao = QuestaoHistoricoUsuario.builder()
                    .usuario(simulado.getUsuario())
                    .questao(qs.getQuestao())
                    .data(agora)
                    .acertou(correta)
                    .simuladoId(simulado.getId())
                    .nivelDificuldade(qs.getQuestao().getNivelDificuldade())
                    .build();

            historicos.add(historicoQuestao); // adiciona à lista

            if (correta) acertos++;
            totalPontuacao += qs.getPontuacaoObtida();
        }

        // ✅ Salva todos os históricos de uma vez (melhor performance)
        questaoHistoricoUsuarioRepository.saveAll(historicos);

        double pontuacaoFinal = (acertos * 100.0) / request.getRespostas().size();
        simulado.setPontuacaoFinal(pontuacaoFinal);
        simulado.setStatus(StatusSimulado.FINALIZADO);
        simuladoRepository.save(simulado);

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
        Simulado simulado = simuladoRepository.findById(simuladoId).orElseThrow(() -> new RuntimeException("Simulado não encontrado"));

        if (!StatusSimulado.FINALIZADO.equals(simulado.getStatus())) {
            throw new BusinessException("O resultado só pode ser visualizado após o simulado ser finalizado.");
        }

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

            return PacoteFiltroSimuladoDTO.builder().pacoteId(pacote.getId()).nomePacote(pacote.getNome()).concursoId(pacote.getConcurso().getId()).nomeConcurso(pacote.getConcurso().getNome()).versao(pacote.getVersao()).temas(temas).bancasDisponiveis(bancas).niveisDisponiveis(niveis).build();
        }).toList();
    }

}

