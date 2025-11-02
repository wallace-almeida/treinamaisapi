package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.questao.request.EstruturaLoteRequest;
import com.treinamaisapi.common.dto.questao.request.TemaLoteRequest;
import com.treinamaisapi.common.dto.questao.request.TemaRequest;
import com.treinamaisapi.common.dto.questao.response.TemaResponse;
import com.treinamaisapi.common.dto.usuario.UsuarioRequest;
import com.treinamaisapi.entity.tema.Tema;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;
import java.util.Map;

@Tag(name = "Tema", description = "Usuarios do sistema")
public interface TemaControllerSwagger {


    @PostMapping("/create")
    ResponseEntity<TemaResponse> criar(@RequestBody TemaRequest request);

    // Inserção em lote simples
    @PostMapping("/lote")
    Map<String, List<String>> criarTemasLote(@RequestBody TemaLoteRequest request);

    // Inserção em lote hierárquico
    @PostMapping("/estrutura/lote")
    Map<String, List<String>> criarEstrutura(@RequestBody EstruturaLoteRequest request);

    @GetMapping
    ResponseEntity<List<TemaResponse>> listar();

    @GetMapping("/disponiveis/{usuarioId}")
    List<Tema> listarTemasDisponiveis(@PathVariable Long usuarioId);
}
