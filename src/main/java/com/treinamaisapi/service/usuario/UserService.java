package com.treinamaisapi.service.usuario;


import com.treinamaisapi.common.dto.usuario.UsuarioRequest;
import com.treinamaisapi.common.dto.usuario.UsuarioResponse;
import com.treinamaisapi.common.dto.usuario.progress.ProgressoUsuarioResponse;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.HistoricoEstudoRepository;
import com.treinamaisapi.repository.QuestaoHistoricoUsuarioRepository;
import com.treinamaisapi.repository.UsuarioRepository;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class UserService {
    private final BCryptPasswordEncoder passwordEncoder = new BCryptPasswordEncoder();
    private final UsuarioRepository usuarioRepository;
    private final QuestaoHistoricoUsuarioRepository questaoHistoricoUsuarioRepository;
    private final HistoricoEstudoRepository historicoEstudoRepository;

    public UserService(UsuarioRepository usuarioRepository, QuestaoHistoricoUsuarioRepository questaoHistoricoUsuarioRepository, HistoricoEstudoRepository historicoEstudoRepository) {
        this.usuarioRepository = usuarioRepository;
        this.questaoHistoricoUsuarioRepository = questaoHistoricoUsuarioRepository;
        this.historicoEstudoRepository = historicoEstudoRepository;
    }

    public Usuario findByEmail(String email) {
        Optional<Usuario> usuario = usuarioRepository.findByEmail(email);
        return usuario.orElse(null);
    }

    public void criarUsuario(UsuarioRequest request) {
        if (usuarioRepository.existsByEmail(request.getEmail())) {
            throw new RuntimeException("E-mail já cadastrado");
        }
        Usuario usuario = new Usuario();
        usuario.setNome(request.getNome());
        usuario.setEmail(request.getEmail());
        usuario.setSenha(passwordEncoder.encode(request.getSenha()));

        usuarioRepository.save(usuario);

    }

    // Progresso do Usuario no simulado
    public ProgressoUsuarioResponse obterProgresso(Long usuarioId) {

        Long totalQuestoes = questaoHistoricoUsuarioRepository
                .countByUsuarioId(usuarioId);

        Long totalAcertos = questaoHistoricoUsuarioRepository
                .countByUsuarioIdAndAcertouTrue(usuarioId);

        double aproveitamento = totalQuestoes == 0
                ? 0.0
                : (totalAcertos * 100.0) / totalQuestoes;

        Long minutos = historicoEstudoRepository
                .sumTempoEstudoByUsuario(usuarioId);

        return ProgressoUsuarioResponse.builder()
                .questoesResolvidas(totalQuestoes)
                .aproveitamento(aproveitamento)
                .tempoEstudo(formatarTempo(minutos))
                .build();
    }

    private String formatarTempo(Long minutos) {
        long horas = minutos / 60;
        long resto = minutos % 60;
        return horas + "h " + resto + "m";
    }
}


