package com.treinamaisapi.service.desempenho;

import com.treinamaisapi.common.dto.desempenho.DesempenhoPorMateriaResponse;
import com.treinamaisapi.common.dto.desempenho.DesempenhoUsuarioResponse;
import com.treinamaisapi.common.dto.desempenho.EvolucaoAcertosResponse;
import com.treinamaisapi.common.exception.NotFoundException;
import com.treinamaisapi.entity.pontuacao.Pontuacao;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.HistoricoEstudoRepository;
import com.treinamaisapi.repository.PontuacaoRepository;
import com.treinamaisapi.repository.QuestaoHistoricoUsuarioRepository;
import com.treinamaisapi.repository.UsuarioRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

@Service
@RequiredArgsConstructor
public class DesempenhoService {

    private final PontuacaoRepository pontuacaoRepository;
    private final HistoricoEstudoRepository historicoEstudoRepository;
    private final QuestaoHistoricoUsuarioRepository questaoRepo;
    private  final UsuarioRepository usuarioRepository;

    public DesempenhoUsuarioResponse obterDesempenho(Long usuarioId) {

        Usuario usuario = usuarioRepository.findById(usuarioId)
                .orElseThrow(() -> new NotFoundException("Usuário não encontrado"));

        // 🔹 Pontuação
        Pontuacao pontuacao = pontuacaoRepository
                .findByUsuario(usuario)
                .orElse(Pontuacao.nova(usuario));

        // 🔹 Questões
        Long totalQuestoes = questaoRepo.countByUsuarioId(usuarioId);
        Long totalAcertos = questaoRepo.countByUsuarioIdAndAcertouTrue(usuarioId);

        double taxaAcerto = totalQuestoes == 0
                ? 0
                : (totalAcertos * 100.0) / totalQuestoes;

        // 🔹 Tempo
        Long minutos = historicoEstudoRepository.sumTempoEstudoByUsuario(usuarioId);
        String tempoFormatado = formatarTempo(minutos);

        // 🔹 Dias ativos
        Long diasAtivos = questaoRepo.countDiasAtivos(usuarioId);

        // 🔹 Evolução (30 dias)
        LocalDateTime inicio = LocalDateTime.now().minusDays(30);
        List<EvolucaoAcertosResponse> evolucao =
                questaoRepo.evolucaoAcertos(usuarioId, inicio)
                        .stream()
                        .map(r -> new EvolucaoAcertosResponse(
                                r[0].toString(),
                                (Double) r[1]
                        ))
                        .toList();

        // 🔹 Por matéria
        List<DesempenhoPorMateriaResponse> porMateria =
                questaoRepo.desempenhoPorMateria(usuarioId)
                        .stream()
                        .map(r -> new DesempenhoPorMateriaResponse(
                                (String) r[0],
                                (Double) r[1]
                        ))
                        .toList();

        return DesempenhoUsuarioResponse.builder()
                .nome(usuario.getNome())
                .nivel(pontuacao.getNivelAtual())
                .xpTotal(pontuacao.getTotal())
                .tituloNivel(tituloNivel(pontuacao.getNivelAtual()))
                .questoesResolvidas(totalQuestoes)
                .taxaAcerto(taxaAcerto)
                .tempoEstudo(tempoFormatado)
                .diasAtivos(diasAtivos)
                .evolucao(evolucao)
                .porMateria(porMateria)
                .build();
    }

    private String formatarTempo(Long minutos) {
        if (minutos == null) return "0h 0m";
        return (minutos / 60) + "h " + (minutos % 60) + "m";
    }

    private String tituloNivel(int nivel) {
        if (nivel < 5) return "Iniciante";
        if (nivel < 15) return "Intermediário";
        return "Avançado";
    }
}
