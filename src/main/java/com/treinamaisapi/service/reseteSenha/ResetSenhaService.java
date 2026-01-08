package com.treinamaisapi.service.reseteSenha;

import com.treinamaisapi.entity.resetSenha.ResetSenhaToken;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.ResetSenhaTokenRepository;
import com.treinamaisapi.repository.UsuarioRepository;
import com.treinamaisapi.service.email.EmailService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Random;
import java.util.UUID;

@Service
public class ResetSenhaService {

    @Autowired
    private UsuarioRepository usuarioRepository;

    @Autowired
    private ResetSenhaTokenRepository tokenRepository;

    @Autowired
    private EmailService emailService;

    @Autowired
    private PasswordEncoder passwordEncoder;

    // 1️⃣ Solicitar código
    public void solicitarCodigo(String email) {
        Usuario usuario = usuarioRepository.findByEmail(email)
                .orElseThrow(() -> new RuntimeException("Usuário não encontrado"));

        // Gera código de 6 dígitos
        String codigo = String.format("%06d", new Random().nextInt(999999));
        String token = UUID.randomUUID().toString();

        // Cria token de reset
        ResetSenhaToken reset = new ResetSenhaToken();
        reset.setCodigo(codigo);
        reset.setToken(token);
        reset.setUsuario(usuario);
        reset.setExpiracao(LocalDateTime.now().plusMinutes(15));

        tokenRepository.save(reset);

        // Envia email usando EmailService correto
        emailService.enviarEmail(
                email,                               // destinatário
                "Redefinição de senha",              // assunto
                "Olá " + usuario.getNome() + "!\n\n" +
                        "Seu código de redefinição de senha é: " + codigo + "\n" +
                        "Este código expira em 15 minutos.\n\n" +
                        "Se você não solicitou, ignore este email.\n\n" +
                        "Atenciosamente,\nEquipe TreinaMais"
        );
    }

    // 2️⃣ Confirmar código (RETORNA TOKEN)
    public String confirmarCodigo(String email, String codigo) {
        ResetSenhaToken reset = tokenRepository
                .findByUsuarioEmailAndCodigoAndUsadoFalse(email, codigo)
                .orElseThrow(() -> new RuntimeException("Código inválido"));

        if (reset.getExpiracao().isBefore(LocalDateTime.now())) {
            throw new RuntimeException("Código expirado");
        }

        return reset.getToken();
    }

    // 3️⃣ Redefinir senha
    public void redefinirSenha(String token, String novaSenha) {
        ResetSenhaToken reset = tokenRepository
                .findByTokenAndUsadoFalse(token)
                .orElseThrow(() -> new RuntimeException("Token inválido"));

        Usuario usuario = reset.getUsuario();
        usuario.setSenha(passwordEncoder.encode(novaSenha));

        usuarioRepository.save(usuario);

        reset.setUsado(true);
        tokenRepository.save(reset);
    }
}
