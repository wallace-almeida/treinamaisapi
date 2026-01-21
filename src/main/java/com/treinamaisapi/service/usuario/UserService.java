package com.treinamaisapi.service.usuario;


import com.treinamaisapi.common.dto.usuario.UsuarioRequest;
import com.treinamaisapi.common.dto.usuario.perfil.AtualizarPerfilRequest;
import com.treinamaisapi.common.dto.usuario.perfil.UsuarioPerfilResponse;
import com.treinamaisapi.common.dto.usuario.progress.ProgressoUsuarioResponse;
import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.common.exception.NotFoundException;
import com.treinamaisapi.entity.enums.AvatarPermitido;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.HistoricoEstudoRepository;
import com.treinamaisapi.repository.QuestaoHistoricoUsuarioRepository;
import com.treinamaisapi.repository.SimuladoRepository;
import com.treinamaisapi.repository.UsuarioRepository;
import com.treinamaisapi.service.avatar.AvatarService;
import com.treinamaisapi.service.serviceAcess.RateLimitService;
import jakarta.transaction.Transactional;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class UserService {
    private final BCryptPasswordEncoder passwordEncoder = new BCryptPasswordEncoder();
    private final UsuarioRepository usuarioRepository;
    private final QuestaoHistoricoUsuarioRepository questaoHistoricoUsuarioRepository;
    private final HistoricoEstudoRepository historicoEstudoRepository;
    private final SimuladoRepository simuladoRepository;
    private  final AvatarService avatarService;

    private  final RateLimitService rateLimitService;

    public UserService(UsuarioRepository usuarioRepository, QuestaoHistoricoUsuarioRepository questaoHistoricoUsuarioRepository, HistoricoEstudoRepository historicoEstudoRepository, SimuladoRepository simuladoRepository, AvatarService avatarService, RateLimitService rateLimitService) {
        this.usuarioRepository = usuarioRepository;
        this.questaoHistoricoUsuarioRepository = questaoHistoricoUsuarioRepository;
        this.historicoEstudoRepository = historicoEstudoRepository;
        this.simuladoRepository = simuladoRepository;
        this.avatarService = avatarService;
        this.rateLimitService = rateLimitService;
    }

    public Usuario findByEmail(String email) {
        Optional<Usuario> usuario = usuarioRepository.findByEmail(email);
        return usuario.orElse(null);
    }

    public void criarUsuario(UsuarioRequest request) {
        if (usuarioRepository.existsByEmail(request.getEmail())) {
            throw new BusinessException("E-mail já cadastrado");
        }

        Usuario usuario = Usuario.builder()
                .nome(request.getNome())
                .email(request.getEmail())
                .senha(passwordEncoder.encode(request.getSenha()))
                .avatar("avatar_01") // mudar depois
                .build();

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

        Long minutos =
                historicoEstudoRepository.sumTempoEstudoByUsuario(usuarioId);

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

    public void atualizarAvatar(Long usuarioId, String avatarNome) {

        Usuario usuario = usuarioRepository.findById(usuarioId)
                .orElseThrow(() ->
                        new NotFoundException("Usuário não encontrado")
                );

        // Validação simples (opcional, mas recomendada)
        if (!AvatarPermitido.isValido(avatarNome)) {
            throw new BusinessException("Avatar inválido");
        }

        usuario.setAvatar(avatarNome);
        usuarioRepository.save(usuario);
    }


    @Transactional
    public UsuarioPerfilResponse atualizarPerfil(
            Long usuarioId,
            AtualizarPerfilRequest request
    ) {
        Usuario usuario = usuarioRepository.findById(usuarioId)
                .orElseThrow(() -> new NotFoundException("Usuário não encontrado"));

        // 1️⃣ Rate limit (antes de validar senha)
        rateLimitService.validar(usuarioId);

        // 2️⃣ Senha sempre obrigatória
        if (!passwordEncoder.matches(request.getSenhaAtual(), usuario.getSenha())) {
            throw new BusinessException("Senha inválida");
        }

        // 3️⃣ Senha correta → reseta rate limit
        rateLimitService.resetar(usuarioId);

        boolean alterou = false;

        // 4️⃣ Atualiza nome (se veio)
        if (request.getName() != null &&
                !request.getName().equals(usuario.getNome())) {

            usuario.setNome(request.getName());
            alterou = true;
        }

        // 5️⃣ Atualiza email (se veio)
        if (request.getEmail() != null &&
                !request.getEmail().equals(usuario.getEmail())) {

            if (usuarioRepository.existsByEmail(request.getEmail())) {
                throw new BusinessException("E-mail já está em uso");
            }

            usuario.setEmail(request.getEmail());
            alterou = true;
        }

        if (!alterou) {
            throw new BusinessException("Nenhuma alteração detectada");
        }

        usuarioRepository.save(usuario);

        return UsuarioPerfilResponse.fromEntity(usuario);
    }




}


